#include <map>
#include <string>
#include <vector>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <chrono>
#include <sstream>
#include <functional>
#include <list>
#include <fstream>
#include <unordered_map>
#include <deque>
#include <boost/range/iterator_range.hpp>
#include <boost/range/istream_range.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/program_options.hpp>
#include <boost/system/system_error.hpp>

#include <aio.h>

#include "common.hpp"
#include "load_config.hpp"
#include "posixapi.hpp"
#include "yield.hpp"

#define noexcept
#define override

namespace wandbox {

	namespace brange = boost::range;
	namespace ptime = boost::posix_time;

	using std::shared_ptr;
	using std::unique_ptr;
	using std::make_shared;
	using std::string;
	using std::vector;
	using std::list;
	using std::size_t;
	using std::move;
	using std::ref;
	using std::cref;
	using boost::system::error_code;
	using std::placeholders::_1;
	using std::placeholders::_2;
	using std::placeholders::_3;

	struct end_read_condition {
		explicit end_read_condition(std::size_t min = 0): min(min) { }
		template <typename Iter>
		std::pair<Iter, bool> operator ()(Iter first, Iter last) const {
			const auto d = std::distance(first, last);
			if (d < min) return { first, false };
			if (d >= BUFSIZ) return { last, true };
			const auto ite = std::find(first, last, '\n');
			return { ite, ite != last };
		}
		std::size_t min;
	};
}

namespace boost {
	namespace asio {
		template <>
		struct is_match_condition<wandbox::end_read_condition>: boost::mpl::true_ { };
	}
}

namespace wandbox {

	string ptracer;
	string config_file;
	server_config config;

	struct unique_child_pid {
		explicit unique_child_pid(pid_t pid = 0): pid(pid), st(0), waited(false) { }
		unique_child_pid(const unique_child_pid &) = delete;
		unique_child_pid(unique_child_pid &&other): pid(0), st(0), waited(false) {
			std::swap(pid, other.pid);
			std::swap(st, other.st);
			std::swap(waited, other.waited);
		}
		unique_child_pid &operator =(const unique_child_pid &) = delete;
		unique_child_pid &operator =(unique_child_pid &&other) {
			std::swap(pid, other.pid);
			std::swap(st, other.st);
			std::swap(waited, other.waited);
			if (pid != other.pid) other.do_wait();
			other.pid = 0;
			other.st = 0;
			other.waited = false;
			return *this;
		}
		~unique_child_pid() {
			do_wait();
		}
		int wait() {
			return do_wait();
		}
		int wait_nonblock() {
			return do_wait(WNOHANG);
		}
		pid_t get() const noexcept { return pid; }
		bool finished() const noexcept { return waited; }
		bool empty() const noexcept { return pid == 0; }
	private:
		int do_wait(int flag = 0) {
			if (waited) return st;
			if (pid == 0) return 0;
			if (::waitpid(pid, &st, flag) <= 0) return 0;
			waited = true;
			return st;
		}
		pid_t pid;
		int st;
		bool waited;
	};

	std::shared_ptr<DIR> make_tmpdir(const std::string &seed) {
		while (true) {
			try {
				return opendir(mkdtemp(seed));
			} catch (std::system_error &e) {
				if (e.code().value() != ENOTDIR) throw;
			}
		}
	}

	struct socket_write_buffer: std::enable_shared_from_this<socket_write_buffer> {
		socket_write_buffer(std::shared_ptr<tcp::socket> sock)
			 : sock(sock),
			   front_handlers(),
			   back_handlers(),
			   front_buf(),
			   back_buf(),
			   writing(false),
			   mtx()
		{ }
		template <typename Handler>
		void async_write_command(std::string cmd, std::string data, Handler &&handler) {
			std::unique_lock<std::recursive_mutex> l(mtx);
			data = encode_qp(std::move(data));
			back_buf.emplace_back(cmd + " " + std::to_string(data.length()) + ":" + data + "\n");
			back_handlers.emplace_back(std::forward<Handler>(handler));
			flush();
		}
		void on_wrote() {
			std::unique_lock<std::recursive_mutex> l(mtx);
			for (const auto &x: front_handlers) x();
			writing = false;
			if (!back_handlers.empty()) flush();
		}
		void flush() {
			std::unique_lock<std::recursive_mutex> l(mtx);
			if (!writing) {
				std::swap(front_handlers, back_handlers);
				std::swap(front_buf, back_buf);
				std::vector<asio::const_buffer> b;
				for (const auto &x: front_buf) {
					b.push_back(asio::buffer(x));
				}
				async_write(*sock, b, std::bind<void>(std::mem_fn(&socket_write_buffer::on_wrote), shared_from_this()));
				back_buf = {};
				back_handlers = {};
				writing = true;
			}
		}

		std::shared_ptr<tcp::socket> sock;
		std::vector<std::function<void()>> front_handlers;
		std::vector<std::function<void()>> back_handlers;
		std::vector<std::string> front_buf;
		std::vector<std::string> back_buf;
		bool writing;
		std::recursive_mutex mtx;
	};

	template <typename T, typename D, typename ...Args>
	std::unique_ptr<T, D> make_unique_with_deleter(D &&d, Args &&...args) {
		return std::unique_ptr<T, D>(new T(std::forward<Args>(args)...), std::forward<D>(d));
	}
	template <typename T, typename ...Args>
	std::unique_ptr<T> make_unique(Args &&...args) {
		return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
	}

	static const compiler_trait &get_compiler(const std::unordered_map<std::string, std::string> &received) {
		return *config.compilers.get<1>().find([&]() -> string {
			std::string ret;
			const auto &s = received.at("Control");
			auto ite = s.begin();
			qi::parse(ite, s.end(), "compiler=" >> *qi::char_, ret);
			return ret;
		}());
	}

	struct program_runner: private coroutine {
		typedef void result_type;
		struct command_type {
			std::vector<std::string> arguments;
			std::string stdin_command;
			std::string stdout_command;
			std::string stderr_command;
			int soft_kill_wait;
		};
		struct pipe_forwarder_base: boost::noncopyable {
			virtual bool closed() const noexcept = 0;
			virtual void async_forward(std::function<void ()>) noexcept = 0;
		};
		struct status_forwarder: pipe_forwarder_base {
			status_forwarder(std::shared_ptr<asio::io_service> aio, std::shared_ptr<asio::signal_set> sigs, unique_child_pid &&pid)
				 : aio(aio),
				   sigs(sigs),
				   pid(std::move(pid))
			{ }
			bool closed() const noexcept override {
				return pid.finished();
			}
			void async_forward(std::function<void ()> handler) noexcept override {
				sigs->async_wait(std::bind<void>(&status_forwarder::wait_handler, std::ref(*this), handler));
			}
			int get_status() noexcept {
				return pid.wait_nonblock();
			}
			void kill(int signo) noexcept {
				if (!pid.finished()) ::kill(pid.get(), signo);
			}
			void wait_handler(std::function<void ()> handler) {
				pid.wait_nonblock();
				if (not pid.finished()) async_forward(handler);
				else handler();
			}
			std::shared_ptr<asio::io_service> aio;
			std::shared_ptr<asio::signal_set> sigs;
			unique_child_pid pid;
		};
		struct input_forwarder: pipe_forwarder_base {
			input_forwarder(std::shared_ptr<asio::io_service> aio, unique_fd &&fd, std::string input)
				 : aio(aio),
				   pipe(*aio),
				   input(std::move(input))
			{
				pipe.assign(fd.get());
				fd.release();
			}
			bool closed() const noexcept override {
				return !pipe.is_open();
			}
			void async_forward(std::function<void ()> handler) noexcept override {
				async_write(pipe, asio::buffer(input), std::bind<void>(&input_forwarder::on_wrote, std::ref(*this), handler));
			}
			void on_wrote(std::function<void ()> handler) {
				pipe.close();
				handler();
			}
			std::shared_ptr<asio::io_service> aio;
			asio::posix::stream_descriptor pipe;
			std::string input;
		};
		struct write_limit_counter {
			explicit write_limit_counter(std::size_t soft_limit, std::size_t hard_limit)
				 : soft_limit(soft_limit),
				   hard_limit(hard_limit),
				   current(0),
				   proc(nullptr) { }
			void set_process(std::shared_ptr<status_forwarder> proc) { this->proc = proc; }
			void add(std::size_t len) {
				if (std::numeric_limits<std::size_t>::max() - len < current) current = std::numeric_limits<std::size_t>::max();
				else current += len;
				if (hard_limit < current) {
					if (proc) proc->kill(SIGKILL);
				} else if (soft_limit < current) {
					if (proc) proc->kill(SIGXFSZ);
				}
			}
			std::size_t soft_limit, hard_limit, current;
			std::shared_ptr<status_forwarder> proc;
		};
		struct output_forwarder: pipe_forwarder_base, private coroutine {
			output_forwarder(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, unique_fd &&fd, std::string command, std::shared_ptr<write_limit_counter> limit)
				 : aio(aio),
				   sockbuf(std::make_shared<socket_write_buffer>(sock)),
				   pipe(*aio),
				   command(std::move(command)),
				   buf(),
				   limit(limit)
			{
				pipe.assign(fd.get());
				fd.release();
			}
			bool closed() const noexcept override {
				return !pipe.is_open();
			}
			void async_forward(std::function<void ()> handler) noexcept override {
				this->handler = handler;
				(*this)();
			}
			void operator ()(error_code ec = error_code(), std::size_t len = 0) {
				reenter (this) while (true) {
					buf.resize(BUFSIZ);
					yield pipe.async_read_some(asio::buffer(buf), std::ref(*this));
					if (ec) {
						pipe.close();
						handler();
						yield break;
					}
					yield {
						std::string t(buf.begin(), buf.begin() + len);
						sockbuf->async_write_command(command, std::move(t), std::ref(*this));
						limit->add(len);
					}
				}
			}
			std::shared_ptr<asio::io_service> aio;
			std::shared_ptr<socket_write_buffer> sockbuf;
			asio::posix::stream_descriptor pipe;
			std::string command;
			std::vector<char> buf;
			std::function<void ()> handler;
			std::shared_ptr<write_limit_counter> limit;
		};

		program_runner(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, std::unordered_map<std::string, std::string> received, std::shared_ptr<asio::signal_set> sigs, std::shared_ptr<DIR> workdir)
			 : aio(aio),
			   strand(std::make_shared<asio::io_service::strand>(*aio)),
			   sock(sock),
			   sockbuf(std::make_shared<socket_write_buffer>(sock)),
			   received(std::move(received)),
			   sigs(sigs),
			   workdir(workdir),
			   pipes(),
			   kill_timer(std::make_shared<asio::deadline_timer>(*aio)),
			   limitter(std::make_shared<write_limit_counter>(config.jail.output_limit_warn, config.jail.output_limit_kill)),
			   laststatus(0)
		{
		}
		program_runner(const program_runner &) = default;
		program_runner &operator =(const program_runner &) = default;
		program_runner(program_runner &&) = default;
		program_runner &operator =(program_runner &&) = default;

		void operator ()(error_code ec = error_code(), size_t len = 0) {
			reenter (this) {

				{
					namespace qi = boost::spirit::qi;
					const auto &compiler = get_compiler(received);

					std::vector<std::string> args = compiler.compile_command;

					const auto it = received.find("CompilerOption");
					if (it != received.end()) {
						std::unordered_set<std::string> selected_switches;
						{
							auto ite = it->second.begin();
							qi::parse(ite, it->second.end(), qi::as_string[+(qi::char_-','-'\n')] % ',', selected_switches);
						}

						for (const auto &sw: compiler.switches) {
							if (selected_switches.count(sw.name) != 0) {
								args.insert(args.end(), sw.flags.begin(), sw.flags.end());
							}
						}
					}
					commands = {
						{ std::move(args), "", "CompilerMessageS", "CompilerMessageE", config.jail.compile_time_limit },
						{ compiler.run_command, "StdIn", "StdOut", "StdErr", config.jail.program_duration }
					};
				}

				yield sockbuf->async_write_command("Control", "Start", strand->wrap(*this));

				while (!commands.empty()) {
					current = std::move(commands.front());
					commands.pop_front();
					{
						auto c = piped_spawn(_P_NOWAIT, workdir, current.arguments);
						unique_child_pid pid(c.pid);

						pipes = {
							std::make_shared<input_forwarder>(aio, std::move(c.fd_stdin), received[current.stdin_command]),
							std::make_shared<output_forwarder>(aio, sock, std::move(c.fd_stdout), current.stdout_command, limitter),
							std::make_shared<output_forwarder>(aio, sock, std::move(c.fd_stderr), current.stderr_command, limitter),
							std::make_shared<status_forwarder>(aio, sigs, std::move(pid)),
						};
						limitter->set_process(std::static_pointer_cast<status_forwarder>(pipes[3]));
					}
					fork pipes[0]->async_forward(strand->wrap(*this));
					if (is_child()) goto wait_process_killed;
					fork pipes[1]->async_forward(strand->wrap(*this));
					if (is_child()) goto wait_process_killed;
					fork pipes[2]->async_forward(strand->wrap(*this));
					if (is_child()) goto wait_process_killed;
					fork pipes[3]->async_forward(strand->wrap(*this));
					if (is_child()) goto wait_process_killed;

					kill_timer->expires_from_now(ptime::seconds(current.soft_kill_wait));
					yield kill_timer->async_wait(strand->wrap(*this));
					if (ec) yield break;
					std::static_pointer_cast<status_forwarder>(pipes[3])->kill(SIGXCPU);

					kill_timer->expires_from_now(ptime::seconds(config.jail.kill_wait));
					yield kill_timer->async_wait(strand->wrap(*this));
					if (ec) yield break;
					std::static_pointer_cast<status_forwarder>(pipes[3])->kill(SIGKILL);

					yield break;

				wait_process_killed:
					if (not std::all_of(pipes.begin(), pipes.end(), [](std::shared_ptr<pipe_forwarder_base> p) { return p->closed(); })) yield break;
					kill_timer->cancel(ec);
					laststatus = std::static_pointer_cast<status_forwarder>(pipes[3])->get_status();
					if (!WIFEXITED(laststatus) || (WEXITSTATUS(laststatus) != 0)) break;
				}
				if (WIFEXITED(laststatus)) yield sockbuf->async_write_command("ExitCode", std::to_string(WEXITSTATUS(laststatus)), strand->wrap(*this));
				if (WIFSIGNALED(laststatus)) yield sockbuf->async_write_command("Signal", ::strsignal(WTERMSIG(laststatus)), strand->wrap(*this));
				yield sockbuf->async_write_command("Control", "Finish", strand->wrap(*this));
			}
		}

		std::shared_ptr<asio::io_service> aio;
		std::shared_ptr<asio::io_service::strand> strand;
		std::shared_ptr<tcp::socket> sock;
		std::shared_ptr<socket_write_buffer> sockbuf;
		std::unordered_map<std::string, std::string> received;
		std::shared_ptr<asio::signal_set> sigs;
		std::shared_ptr<DIR> workdir;
		std::deque<command_type> commands;
		command_type current;
		std::vector<std::shared_ptr<pipe_forwarder_base>> pipes;
		std::shared_ptr<asio::deadline_timer> kill_timer;
		std::shared_ptr<write_limit_counter> limitter;
		int laststatus;
	};

	struct program_writer: private coroutine {
		typedef void result_type;
		program_writer(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, std::shared_ptr<asio::signal_set> sigs, std::unordered_map<std::string, std::string> received)
			 : aio(aio),
			   sock(sock),
			   src_text(make_unique<std::string>(received["Source"])),
			   src_filename("prog" + get_compiler(received).source_suffix),
			   file(make_unique<asio::posix::stream_descriptor>(*aio)),
			   sigs(sigs),
			   workdir(make_tmpdir("wandboxXXXXXX")),
			   received(std::move(received)),
			   aiocb(std::make_shared<struct aiocb>())
		{
		}
		program_writer(const program_writer &) = default;
		program_writer &operator =(const program_writer &) = default;
		program_writer(program_writer &&) = default;
		program_writer &operator =(program_writer &&) = default;
		void operator ()(error_code ec = error_code(), size_t len = 0) {
			reenter (this) {
				::memset(aiocb.get(), 0, sizeof(*aiocb.get()));
				while (true) {
					aiocb->aio_fildes = ::openat(::dirfd(workdir.get()), src_filename.c_str(), O_WRONLY|O_CLOEXEC|O_CREAT|O_TRUNC|O_EXCL|O_NOATIME, 0600);
					if (aiocb->aio_fildes == -1) {
						if (errno == EAGAIN || errno == EMFILE || errno == EWOULDBLOCK) yield sigs->async_wait(*this);
						else yield break;
					} else {
						break;
					}
				}
				aiocb->aio_buf = const_cast<volatile void *>(static_cast<const volatile void *>(src_text->c_str()));
				aiocb->aio_nbytes = src_text->length();
				aiocb->aio_sigevent.sigev_notify = SIGEV_SIGNAL;
				aiocb->aio_sigevent.sigev_signo = SIGHUP;
				::aio_write(aiocb.get());
				do {
					yield yield sigs->async_wait(*this);
				} while (::aio_error(aiocb.get()) == EINPROGRESS) ;
				::close(aiocb->aio_fildes);
				return program_runner(aio, std::move(sock), std::move(received), std::move(sigs), std::move(workdir))();
			}
		}
		std::shared_ptr<asio::io_service> aio;
		std::shared_ptr<tcp::socket> sock;
		std::shared_ptr<std::string> src_text;
		std::string src_filename;
		std::shared_ptr<asio::posix::stream_descriptor> file;
		std::shared_ptr<asio::signal_set> sigs;
		std::shared_ptr<DIR> workdir;
		std::unordered_map<std::string, std::string> received;
		std::shared_ptr<struct aiocb> aiocb;
	};

	struct version_sender: private coroutine {
		typedef void result_type;
		version_sender(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, std::shared_ptr<asio::signal_set> sigs)
			 : aio(aio),
			   sock(sock),
			   sockbuf(std::make_shared<socket_write_buffer>(sock)),
			   pipe_stdout(nullptr),
			   sigs(sigs),
			   commands(),
			   current(nullptr),
			   child(nullptr),
			   line(),
			   buf(nullptr)
		{
			for (const auto &c: config.compilers) commands.push_back(&c);
		}
		version_sender(const version_sender &) = default;
		version_sender &operator =(const version_sender &) = default;
		version_sender(version_sender &&) = default;
		version_sender &operator =(version_sender &&) = default;
		void operator ()(error_code ec = error_code(), size_t len = 0) {
			reenter (this) {
				while (!commands.empty()) {
					current = commands.front();
					commands.pop_front();
					{
						auto c = piped_spawn(_P_NOWAIT, opendir("/"), current->version_command);
						child = std::make_shared<unique_child_pid>(c.pid);
						pipe_stdout = std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stdout.get());
						c.fd_stdout.release();
					}
					do {
						yield sigs->async_wait(*this);
						child->wait_nonblock();
					} while (not child->finished());

					{
						int st = child->wait_nonblock();
						if (!WIFEXITED(st) || (WEXITSTATUS(st) != 0)) continue;
					}

					yield {
						buf = std::make_shared<asio::streambuf>();
						asio::async_read_until(*pipe_stdout, *buf, '\n', *this);
					}
					if (ec) continue;

					{
						std::istream is(buf.get());
						std::string ver;
						if (!getline(is, ver)) continue;
						const auto &c = *current;
						// NOTE: Each variables must not contain <LF> or <COMMA> or <TAB>.
						// <line> ::= name,language,display_name,ver,display_compile_command<switches><LF>
						// <switches> ::= (,name<TAB>flags<TAB>default<TAB>display_name)*
						line += c.name + "," + c.language + "," + c.display_name + "," + ver + "," + c.display_compile_command;
						for (const auto &sw: c.switches) {
							line +=
								"," + sw.name +
								"\t" + boost::algorithm::join(sw.flags, " ") +
								"\t" + (sw.default_ ? "true" : "false") +
								"\t" + sw.display_name;
						}
						line += "\n";
					}
				}
				yield sockbuf->async_write_command("VersionResult", std::move(line), *this);
			}
		}
		std::shared_ptr<asio::io_service> aio;
		std::shared_ptr<tcp::socket> sock;
		std::shared_ptr<socket_write_buffer> sockbuf;
		std::shared_ptr<asio::posix::stream_descriptor> pipe_stdout;
		std::shared_ptr<asio::signal_set> sigs;
		std::deque<const compiler_trait *> commands;
		const compiler_trait *current;
		std::shared_ptr<unique_child_pid> child;
		std::string line;
		std::shared_ptr<asio::streambuf> buf;
	};

	struct compiler_bridge: private coroutine {
		typedef void result_type;
		compiler_bridge(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, std::shared_ptr<asio::signal_set> sigs)
			 : aio(aio),
			   sock(sock),
			   buf(std::make_shared<asio::streambuf>()),
			   sigs(sigs),
			   received()
		{
		}
		void operator ()(error_code ec = error_code(), size_t len = 0) {
			reenter (this) while (true) {
				yield asio::async_read_until(*sock, *buf, end_read_condition(asio::buffer_size(buf->data()) + 1), *this);
				if (ec) return (void)sock->close(ec);

				while (true) {
					const auto begin = asio::buffer_cast<const char *>(buf->data());
					auto ite = begin;
					const auto end = ite + asio::buffer_size(buf->data());

					std::string command;
					int len = 0;
					std::string data;
					if (!qi::parse(ite, end, +(qi::char_ - qi::space) >> qi::omit[*qi::space] >> qi::omit[qi::int_[phx::ref(len) = qi::_1]] >> qi::omit[':'] >> qi::repeat(phx::ref(len))[qi::char_] >> qi::omit[qi::eol], command, data)) break;
					if (command == "Control" && data == "run") return program_writer(aio, std::move(sock), sigs, std::move(received))();
					else if (command == "Version") return version_sender(aio, std::move(sock), sigs)();
					else received[command] += decode_qp(data);
					buf->consume(ite - begin);
				}
			}
		}
		std::shared_ptr<asio::io_service> aio;
		std::shared_ptr<tcp::socket> sock;
		std::shared_ptr<asio::streambuf> buf;
		std::shared_ptr<asio::signal_set> sigs;
		std::unordered_map<std::string, std::string> received;
	};

	struct listener: private coroutine {
		typedef void result_type;
		void operator ()(error_code ec = error_code(), std::shared_ptr<tcp::socket> sock = nullptr) {
			reenter (this) while (true) {
				sock = std::make_shared<tcp::socket>(*aio);
				yield acc->async_accept(*sock, std::bind<void>(*this, _1, sock));
				compiler_bridge(aio, std::move(sock), sigs)();
			}
		}
		template <typename ...Args>
		listener(std::shared_ptr<asio::io_service> aio, Args &&...args)
			 : aio(aio),
			   ep(std::forward<Args>(args)...),
			   acc(std::make_shared<tcp::acceptor>(*aio, ep)),
			   sigs(make_shared<asio::signal_set>(*aio, SIGCHLD, SIGHUP))
		{
			try {
				mkdir(config.jail.basedir, 0700);
			} catch (std::system_error &e) {
				if (e.code().value() != EEXIST) throw;
			}
			basedir = opendir(config.jail.basedir);
			chdir(basedir);
		}
	private:
		std::shared_ptr<asio::io_service> aio;
		tcp::endpoint ep;
		std::shared_ptr<tcp::acceptor> acc;
		std::shared_ptr<asio::signal_set> sigs;
		shared_ptr<DIR> basedir;
	};

}
int main(int argc, char **argv) {
	using namespace wandbox;

	{
		namespace po = boost::program_options;

		{
			string config_file_raw;
			po::options_description opt("options");
			opt.add_options()
				("help,h", "show this help")
				("config,c", po::value<string>(&config_file_raw)->default_value(string(DATADIR) + "/config"), "specify config file")
			;

			po::variables_map vm;
			po::store(po::parse_command_line(argc, argv, opt), vm);
			po::notify(vm);

			if (vm.count("help")) {
				std::cout << opt << std::endl;
				return 0;
			}

			config_file = realpath(config_file_raw);
			if (config_file.empty()) {
				std::cerr << "config file '" << config_file_raw << "'not found" << std::endl;
				return 1;
			}
		}
		std::ifstream f(config_file);
		config = load_config(f);
	} {
		ptracer = realpath(dirname(realpath("/proc/self/exe")) + "/" + config.jail.exe);
	}
	auto aio = std::make_shared<asio::io_service>();
	listener s(aio, boost::asio::ip::tcp::v4(), config.network.listen_port);
	s();
	aio->run();
}
