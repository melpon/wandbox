#include <array>
#include <deque>
#include <functional>
#include <mutex>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/asio.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/optional.hpp>
#include <boost/program_options.hpp>
#include <boost/system/system_error.hpp>

#include <time.h>
#include <locale.h>

#include <aio.h>
#include <syslog.h>
#include <sys/eventfd.h>

#include "quoted_printable.hpp"
#include "load_config.hpp"
#include "posixapi.hpp"
#include "syslogstream.hpp"
#include "yield.hpp"

#define PROTECT_FROM_MOVE(member) const auto member = this->member

#if !defined(__GNUC__) || (__GNUC__ < 4) || (__GNUC__ == 4 && __GCC_MINOR__ < 7)
#define noexcept
#define override
#endif

namespace wandbox {
	namespace asio = boost::asio;
	namespace ptime = boost::posix_time;
	namespace phx = boost::phoenix;
	namespace qi = boost::spirit::qi;

	template <typename KeyT, typename ValueT, typename Hash, typename Pred, typename Alloc, typename ActualKey>
	boost::optional<const ValueT &> map_find(const std::unordered_map<KeyT, ValueT, Hash, Pred, Alloc> &m, ActualKey &&k) {
		const auto ite = m.find(std::forward<ActualKey>(k));
		if (ite == m.end()) return boost::none;
		return ite->second;
	}
	template <typename KeyT, typename ValueT, typename Compare, typename Alloc, typename ActualKey>
	boost::optional<const ValueT &> map_find(const std::map<KeyT, ValueT, Compare, Alloc> &m, ActualKey &&k) {
		const auto ite = m.find(std::forward<ActualKey>(k));
		if (ite == m.end()) return boost::none;
		return ite->second;
	}
	template <int Nth, typename Map, typename KeyT>
	boost::optional<const typename Map::value_type &> map_multi_index_find(const Map &m, KeyT &&k) {
		const auto ite = m.template get<Nth>().find(k);
		if (ite == m.template get<Nth>().end()) return boost::none;
		return *ite;
	}

	using std::size_t;
	using std::move;
	using std::ref;
	using std::cref;
	using boost::system::error_code;
	using std::placeholders::_1;
	using std::placeholders::_2;
	using std::placeholders::_3;
	using boost::asio::ip::tcp;

	server_config config;
	bool be_verbose;

	struct counting_semaphore {
		counting_semaphore(asio::io_service &aio, unsigned count)
			 : aio(aio),
			   des(std::make_shared<asio::posix::stream_descriptor>(aio))
		{
			const int fd = ::eventfd(count, EFD_CLOEXEC|EFD_NONBLOCK|EFD_SEMAPHORE);
			if (fd == -1) throw boost::system::system_error(errno, boost::system::system_category());
			try {
				des->assign(fd);
			} catch (...) {
				close(fd);
				throw;
			}
		}
		counting_semaphore(const counting_semaphore &) = delete;
		counting_semaphore(counting_semaphore &&) = delete;
		counting_semaphore &operator =(const counting_semaphore &) = delete;
		counting_semaphore &operator =(counting_semaphore &&) = delete;
		struct semaphore_object {
			template <typename F>
			semaphore_object(asio::io_service &aio, const std::shared_ptr<asio::posix::stream_descriptor> &des, F &&f): aio(aio), des(des) {
				std::clog << "[" << (void *)this << "]" << " semaphore create" << std::endl;
				const auto b = std::make_shared< std::array<unsigned char, 8> >();
				asio::async_read(*des, asio::buffer(*b), std::bind<void>([](F f, error_code ec, std::shared_ptr<void>) { if (!ec) f(); }, std::forward<F>(f), _1, b));
			}
			semaphore_object(const semaphore_object &) = delete;
			semaphore_object(semaphore_object &&) = delete;
			semaphore_object &operator =(const semaphore_object &) = delete;
			semaphore_object &operator =(semaphore_object &&) = delete;
			~semaphore_object() noexcept try {
				const std::uint64_t b = 1;
				asio::write(*des, asio::buffer(&b, sizeof(b)));
				std::clog << "[" << (void *)this << "]" << " semaphore destroy" << std::endl;
			} catch (...) {
			}
			asio::io_service &aio;
			std::shared_ptr<asio::posix::stream_descriptor> des;
		};
		template <typename F>
		std::shared_ptr<void> async_signal(F &&f) {
			return std::make_shared<semaphore_object>(aio, des, std::forward<F>(f));
		}
	private:
		asio::io_service &aio;
		std::shared_ptr<asio::posix::stream_descriptor> des;
	};

	struct socket_write_buffer: std::enable_shared_from_this<socket_write_buffer> {
		socket_write_buffer(std::shared_ptr<tcp::socket> sock)
			 : sock(move(sock)),
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
			data = quoted_printable::encode(move(data));
			back_buf.emplace_back(cmd + " " + std::to_string(data.length()) + ":" + data + "\n");
			back_handlers.emplace_back(std::forward<Handler>(handler));
			flush();
		}
		void on_wrote() {
			std::unique_lock<std::recursive_mutex> l(mtx);
			for (const auto &x: front_handlers) x();
			writing = false;
			front_buf.clear();
			front_handlers.clear();
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
				 : aio(move(aio)),
				   sigs(move(sigs)),
				   pid(move(pid))
			{ }
			bool closed() const noexcept override {
				return pid.finished();
			}
			void async_forward(std::function<void ()> handler) noexcept override {
				sigs->async_wait(std::bind<void>(&status_forwarder::wait_handler, ref(*this), handler));
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
				 : aio(move(aio)),
				   pipe(*this->aio),
				   input(move(input))
			{
				pipe.assign(fd.get());
				fd.release();
			}
			bool closed() const noexcept override {
				return !pipe.is_open();
			}
			void async_forward(std::function<void ()> handler) noexcept override {
				async_write(pipe, asio::buffer(input), std::bind<void>(&input_forwarder::on_wrote, ref(*this), handler));
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
			explicit write_limit_counter(size_t soft_limit, size_t hard_limit)
				 : soft_limit(soft_limit),
				   hard_limit(hard_limit),
				   current(0),
				   proc() { }
			void set_process(std::shared_ptr<status_forwarder> proc) { this->proc = move(proc); }
			void add(size_t len) {
				if (std::numeric_limits<size_t>::max() - len < current) current = std::numeric_limits<size_t>::max();
				else current += len;
				if (auto p = proc.lock()) {
					if (hard_limit < current) p->kill(SIGKILL);
					else if (soft_limit < current) p->kill(SIGXFSZ);
				}
			}
			size_t soft_limit, hard_limit, current;
			std::weak_ptr<status_forwarder> proc;
		};
		struct output_forwarder: pipe_forwarder_base, private coroutine {
			output_forwarder(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, unique_fd &&fd, std::string command, std::shared_ptr<write_limit_counter> limit)
				 : aio(move(aio)),
				   sockbuf(std::make_shared<socket_write_buffer>(move(sock))),
				   pipe(*this->aio),
				   command(move(command)),
				   buf(),
				   limit(move(limit))
			{
				pipe.assign(fd.get());
				fd.release();
			}
			bool closed() const noexcept override {
				return !pipe.is_open();
			}
			void async_forward(std::function<void ()> handler) noexcept override {
				this->handler = move(handler);
				(*this)();
			}
			void operator ()(error_code ec = error_code(), size_t len = 0) {
				reenter (this) while (true) {
					buf.resize(BUFSIZ);
					yield pipe.async_read_some(asio::buffer(buf), ref(*this));
					if (ec) {
						pipe.close();
						if (handler) aio->post(move(handler));
						handler = {};
						yield break;
					}
					yield {
						std::string t(buf.begin(), buf.begin() + len);
						sockbuf->async_write_command(command, move(t), ref(*this));
						if (auto l = limit.lock()) l->add(len);
					}
				}
			}
			std::shared_ptr<asio::io_service> aio;
			std::shared_ptr<socket_write_buffer> sockbuf;
			asio::posix::stream_descriptor pipe;
			std::string command;
			std::vector<char> buf;
			std::function<void ()> handler;
			std::weak_ptr<write_limit_counter> limit;
		};

		program_runner(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, std::unordered_map<std::string, std::string> received, std::shared_ptr<asio::signal_set> sigs, std::shared_ptr<DIR> workdir, compiler_trait target_compiler,std::shared_ptr<void> semaphore)
			 : aio(move(aio)),
			   strand(std::make_shared<asio::io_service::strand>(*this->aio)),
			   sock(move(sock)),
			   sockbuf(std::make_shared<socket_write_buffer>(this->sock)),
			   received(move(received)),
			   sigs(move(sigs)),
			   workdir(move(workdir)),
			   pipes(),
			   kill_timer(std::make_shared<asio::deadline_timer>(*this->aio)),
			   jail(config.jails.at(target_compiler.jail_name)),
			   limitter(std::make_shared<write_limit_counter>(jail.output_limit_warn, jail.output_limit_kill)),
			   target_compiler(target_compiler),
			   laststatus(0),
			   semaphore(move(semaphore))
		{
		}
		program_runner(const program_runner &) = default;
		program_runner &operator =(const program_runner &) = default;
		program_runner(program_runner &&) = default;
		program_runner &operator =(program_runner &&) = default;

		void operator ()(error_code ec = error_code(), size_t = 0) {
			reenter (this) {
				std::clog << "[" << sock.get() << "]" << "running program with '" << target_compiler.name << "' [" << this << "]" << std::endl;
				{
					namespace qi = boost::spirit::qi;

					auto ccargs = target_compiler.compile_command;
					auto progargs = target_compiler.run_command;

					if (const auto r = map_find(received, "CompilerOption")) {
						std::unordered_set<std::string> selected_switches;
						{
							auto ite = r->begin();
							qi::parse(ite, r->end(), qi::as_string[+(qi::char_-','-'\n')] % ',', selected_switches);
						}
						for (const auto &sw: selected_switches) {
							auto t = map_multi_index_find<1>(target_compiler.local_switches, sw);
							if (!t) t = map_find(config.switches, sw);
							if (!t) continue;
							std::clog << "[" << sock.get() << "]" << "using option " << sw << ':';
							for (auto &&f: t->flags) std::clog << ' ' << f;
							std::clog << std::endl;
							const auto f = [t](std::vector<std::string> &args) {
								if (t->insert_position == 0) {
									args.insert(args.end(), t->flags.begin(), t->flags.end());
								} else {
									args.insert(args.begin() + t->insert_position, t->flags.begin(), t->flags.end());
								}
							};
							f(t->runtime ? progargs : ccargs);
						}
					}

					for (auto &x: { std::make_pair("CompilerOptionRaw", &ccargs), std::make_pair("RuntimeOptionRaw", &progargs) }) {
						if (const auto r = map_find(received, x.first)) {
							std::vector<std::string> s;
							auto input = *r;
							boost::algorithm::replace_all(input, "\r\n", "\n");
							boost::algorithm::split(s, input, boost::is_any_of("\r\n"));
							if (not s.empty() && s.back().empty()) {
								s.pop_back();
							}
							x.second->insert(x.second->end(), s.begin(), s.end());
						}
					}

					ccargs.insert(ccargs.begin(), jail.jail_command.begin(), jail.jail_command.end());
					progargs.insert(progargs.begin(), jail.jail_command.begin(), jail.jail_command.end());
					commands = {
						{ move(ccargs), "", "CompilerMessageS", "CompilerMessageE", jail.compile_time_limit },
						{ move(progargs), "StdIn", "StdOut", "StdErr", jail.program_duration }
					};
				}

				yield {
					PROTECT_FROM_MOVE(strand);
					PROTECT_FROM_MOVE(sockbuf);
					sockbuf->async_write_command("Control", "Start", strand->wrap(move(*this)));
				}

				while (!commands.empty()) {
					current = move(commands.front());
					commands.pop_front();
					std::clog << "exec ";
					for (auto &&s: current.arguments) {
						std::clog << '[' << s << ']' << ' ';
					}
					std::clog << std::endl;
					{
						auto c = piped_spawn(workdir, current.arguments);

						pipes = {
							std::make_shared<input_forwarder>(aio, move(c.fd_stdin), received[current.stdin_command]),
							std::make_shared<output_forwarder>(aio, sock, move(c.fd_stdout), current.stdout_command, limitter),
							std::make_shared<output_forwarder>(aio, sock, move(c.fd_stderr), current.stderr_command, limitter),
							std::make_shared<status_forwarder>(aio, sigs, move(c.pid)),
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
					yield {
						PROTECT_FROM_MOVE(strand);
						PROTECT_FROM_MOVE(kill_timer);
						kill_timer->async_wait(strand->wrap(move(*this)));
					}
					if (ec) yield break;
					std::static_pointer_cast<status_forwarder>(pipes[3])->kill(SIGXCPU);

					kill_timer->expires_from_now(ptime::seconds(jail.kill_wait));
					yield {
						PROTECT_FROM_MOVE(strand);
						PROTECT_FROM_MOVE(kill_timer);
						kill_timer->async_wait(strand->wrap(move(*this)));
					}
					if (ec) yield break;
					std::static_pointer_cast<status_forwarder>(pipes[3])->kill(SIGKILL);

					yield break;

				wait_process_killed:
					if (not std::all_of(pipes.begin(), pipes.end(), [](std::shared_ptr<pipe_forwarder_base> p) { return p->closed(); })) yield break;
					kill_timer->cancel(ec);
					laststatus = std::static_pointer_cast<status_forwarder>(pipes[3])->get_status();
					if (!WIFEXITED(laststatus) || (WEXITSTATUS(laststatus) != 0)) break;
				}
				if (WIFEXITED(laststatus)) yield {
					PROTECT_FROM_MOVE(strand);
					PROTECT_FROM_MOVE(sockbuf);
					sockbuf->async_write_command("ExitCode", std::to_string(WEXITSTATUS(laststatus)), strand->wrap(move(*this)));
				}
				if (WIFSIGNALED(laststatus)) yield {
					PROTECT_FROM_MOVE(strand);
					PROTECT_FROM_MOVE(sockbuf);
					sockbuf->async_write_command("Signal", ::strsignal(WTERMSIG(laststatus)), strand->wrap(move(*this)));
				}
				std::clog << "[" << sock.get() << "]" << "finished [" << this << "]" << std::endl;
				yield {
					PROTECT_FROM_MOVE(strand);
					PROTECT_FROM_MOVE(sockbuf);
					sockbuf->async_write_command("Control", "Finish", strand->wrap(move(*this)));
				}
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
		jail_config jail;
		std::shared_ptr<write_limit_counter> limitter;
		compiler_trait target_compiler;
		int laststatus;
		std::shared_ptr<void> semaphore;
	};

	struct program_writer: private coroutine {
		typedef void result_type;
		program_writer(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, std::shared_ptr<asio::signal_set> sigs, std::unordered_map<std::string, std::string> received, std::unordered_map<std::string, std::string> sources, compiler_trait target_compiler, std::shared_ptr<void> semaphore)
			 : aio(move(aio)),
			   sock(move(sock)),
			   file(std::make_shared<asio::posix::stream_descriptor>(*this->aio)),
			   sigs(move(sigs)),
			   unique_name(),
			   workdir(),
			   received(move(received)),
			   aiocb(std::make_shared<struct aiocb>()),
			   target_compiler(target_compiler),
			   semaphore(move(semaphore))
		{
			while (unique_name.empty() || !workdir) try {
				unique_name = mkdtemp("wandboxXXXXXX");
				workdir = opendir(unique_name);
			} catch (std::system_error &e) {
				if (e.code().value() != ENOTDIR) throw;
			}

			aiocb->aio_fildes = -1;

			char s[64];
			time_t t = ::time(0);
			struct tm l;
			::localtime_r(&t, &l);
			::strftime(s, 64, "%Y%m%d", &l);

			std::clog << "[" << this->sock.get() << "]" << "create log directory '" << (config.system.storedir + "/" + s + "/" + unique_name) << "' [" << this << "]" << std::endl;
			const auto logdir = mkdir_p_open_at({}, config.system.storedir + "/" + s + "/" + unique_name, 0700);
			if (!logdir) {
				std::clog << "[" << this->sock.get() << "]" << "failed to create log directory '" << (config.system.storedir + "/" + s + "/" + unique_name) << "' [" << this << "]" << std::endl;
				throw_system_error(errno);
			}

			std::clog << "[" << this->sock.get() << "]" << "using temporary name '" << unique_name << "' [" << this << "]" << std::endl;
			const auto savedir = mkdir_p_open_at(workdir, "store", 0700);
			if (!savedir) {
				std::clog << "[" << this->sock.get() << "]" << "failed to create working directory '" << unique_name << "' [" << this << "]" << std::endl;
				throw_system_error(errno);
			}

			std::unordered_multimap<std::string, std::shared_ptr<DIR>> dirs;
			dirs.emplace(std::string(), savedir);
			dirs.emplace(std::string(), logdir);
			for (auto &&x: sources) {
				std::clog << "[" << this->sock.get() << "]" << "registering file '" << x.first << "' [" << this << "]" << std::endl;
				if (x.first.empty()) {
					this->sources.emplace_back(this->target_compiler.output_file, x.second, savedir);
					this->sources.emplace_back(this->target_compiler.output_file, x.second, logdir);
				} else {
					auto tree = split_path_tree(x.first);
					if (tree.empty()) continue;
					tree.pop_back();

					for (size_t n = 0; n < tree.size(); ++n) {
						const auto ite = dirs.find(tree[n]);
						if (ite != dirs.end()) continue;

						std::clog << "[" << this->sock.get() << "]" << "create source subdirectory '" << tree[n] << "' [" << this << "]" << std::endl;
						const auto dr = dirs.equal_range(n == 0 ? "" : tree[n-1]);
						const auto dn = n == 0 ? tree[n] : tree[n].substr(tree[n-1].length()+1);
						for (auto pd = dr.first; pd != dr.second; ++pd) {
							dirs.emplace(tree[n], (mkdirat(pd->second, dn, 0700), opendirat(pd->second, dn)));
						}
					}

					const auto filename = x.first.substr(x.first.find_last_of('/')+1);
					const auto dr = dirs.equal_range(tree.empty() ? "" : tree.back());
					for (auto pd = dr.first; pd != dr.second; ++pd) {
						this->sources.emplace_back(filename, x.second, pd->second);
					}
				}
			}
			std::clog << "[" << this->sock.get() << "]" << " ready." << std::endl;
		}
		~program_writer() noexcept {
			if (aiocb && aiocb->aio_fildes != -1) ::close(aiocb->aio_fildes);
		}
		program_writer(const program_writer &) = default;
		program_writer &operator =(const program_writer &) = default;
		program_writer(program_writer &&) = default;
		program_writer &operator =(program_writer &&) = default;
		void operator ()(error_code = error_code(), size_t = 0) {
			reenter (this) {
				while (!sources.empty()) {
					current_source = std::move(sources.front());
					sources.pop_front();
					std::clog << "[" << sock.get() << "]" << "writing file '" << current_source.name << "' [" << this << "]" << std::endl;

					{
						::memset(aiocb.get(), 0, sizeof(*aiocb.get()));
						while (true) {
							aiocb->aio_fildes = ::openat(dirfd(current_source.dir.get()), ("./" + current_source.name).c_str(), O_WRONLY|O_CLOEXEC|O_CREAT|O_TRUNC|O_EXCL|O_NOATIME, 0600);
							if (aiocb->aio_fildes == -1) {
								if (errno == EAGAIN || errno == EMFILE || errno == EWOULDBLOCK) {
									yield {
										PROTECT_FROM_MOVE(sigs);
										sigs->async_wait(move(*this));
									}
								} else {
									std::clog << "[" << sock.get() << "]" << "open failed '" << current_source.name.c_str() << "' [" << this << "]" << std::endl;
									yield break;
								}
							} else {
								break;
							}
						}
						aiocb->aio_buf = current_source.buf;
						aiocb->aio_nbytes = current_source.len;
						aiocb->aio_sigevent.sigev_notify = SIGEV_SIGNAL;
						aiocb->aio_sigevent.sigev_signo = SIGHUP;
						::aio_write(aiocb.get());
						do {
							yield {
								PROTECT_FROM_MOVE(sigs);
								sigs->async_wait(move(*this));
							}
						} while (::aio_error(aiocb.get()) == EINPROGRESS) ;
						::close(aiocb->aio_fildes);
						aiocb->aio_fildes = -1;
					}
					std::clog << "[" << sock.get() << "]" << "write success '" << current_source.name << "' [" << this << "]" << std::endl;
				}
				return program_runner(aio, move(sock), move(received), move(sigs), move(workdir), move(target_compiler), move(semaphore))();
			}
		}
		std::shared_ptr<asio::io_service> aio;
		std::shared_ptr<tcp::socket> sock;
		std::shared_ptr<asio::posix::stream_descriptor> file;
		std::shared_ptr<asio::signal_set> sigs;
		std::string unique_name;
		std::shared_ptr<DIR> workdir;
		std::unordered_map<std::string, std::string> received;
		std::shared_ptr<struct aiocb> aiocb;
		compiler_trait target_compiler;
		std::shared_ptr<void> semaphore;

		struct source_file_t {
			std::string name;
			std::shared_ptr<char> source_shared;
			char *buf;
			std::size_t len;
			std::shared_ptr<DIR> dir;
			source_file_t() = default;
			source_file_t(std::string filename, const std::string &source, std::shared_ptr<DIR> dir)
				: name(std::move(filename)),
				  source_shared(),
				  buf(),
				  len(source.length()),
				  dir(std::move(dir)) {
				source_shared.reset(new char[len], [](char *p) { delete[] p; });
				memcpy(buf = source_shared.get(), source.c_str(), len);
			}
			source_file_t(const source_file_t &) = default;
			source_file_t &operator =(const source_file_t &) = default;
			source_file_t(source_file_t &&) = default;
			source_file_t &operator =(source_file_t &&) = default;
		};
		std::deque<source_file_t> sources;
		source_file_t current_source;
	};

	struct version_sender: private coroutine {
		typedef void result_type;
		version_sender(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, std::shared_ptr<asio::signal_set> sigs, std::shared_ptr<void> semaphore)
			 : aio(move(aio)),
			   sock(move(sock)),
			   sockbuf(std::make_shared<socket_write_buffer>(this->sock)),
			   pipe_stdout(nullptr),
			   sigs(move(sigs)),
			   commands(),
			   current(),
			   child(nullptr),
			   buf(nullptr),
			   semaphore(move(semaphore))
		{
			for (const auto &c: config.compilers) commands.push_back(c);
		}
		version_sender(const version_sender &) = default;
		version_sender &operator =(const version_sender &) = default;
		version_sender(version_sender &&) = default;
		version_sender &operator =(version_sender &&) = default;
		void operator ()(error_code = error_code(), size_t = 0) {
			reenter (this) {
				std::clog << "[" << sock.get() << "]" << "building compiler list" << std::endl;
				while (!commands.empty()) {
					current = move(commands.front());
					commands.pop_front();
					if (current.version_command.empty() || not current.displayable) continue;
					{
						auto c = piped_spawn(opendir("/"), current.version_command);
						child = std::make_shared<unique_child_pid>(move(c.pid));
						pipe_stdout = std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stdout.get());
						c.fd_stdout.release();
					}
					do {
						yield {
							PROTECT_FROM_MOVE(sigs);
							sigs->async_wait(move(*this));
						}
						child->wait_nonblock();
					} while (not child->finished());

					{
						int st = child->wait_nonblock();
						if (!WIFEXITED(st) || (WEXITSTATUS(st) != 0)) continue;
					}

					yield {
						buf = std::make_shared<asio::streambuf>();
						PROTECT_FROM_MOVE(buf);
						PROTECT_FROM_MOVE(pipe_stdout);
						asio::async_read_until(*pipe_stdout, *buf, '\n', move(*this));
					}

					{
						std::istream is(buf.get());
						std::string ver;
						if (!getline(is, ver)) continue;
						versions.emplace_back(generate_displaying_compiler_config(move(current), ver, config.switches));
					}
				}
				yield {
					auto s = "[" + boost::algorithm::join(move(versions), ",") + "]";
					PROTECT_FROM_MOVE(sockbuf);
					sockbuf->async_write_command("VersionResult", move(s), move(*this));
				}
				std::clog << "[" << sock.get() << "]" << "finished [" << this << "]" << std::endl;
			}
		}
		std::shared_ptr<asio::io_service> aio;
		std::shared_ptr<tcp::socket> sock;
		std::shared_ptr<socket_write_buffer> sockbuf;
		std::shared_ptr<asio::posix::stream_descriptor> pipe_stdout;
		std::shared_ptr<asio::signal_set> sigs;
		std::deque<compiler_trait> commands;
		compiler_trait current;
		std::shared_ptr<unique_child_pid> child;
		std::vector<std::string> versions;
		std::shared_ptr<asio::streambuf> buf;
		std::shared_ptr<void> semaphore;
	};

	struct compiler_bridge: private coroutine {
		typedef void result_type;
		compiler_bridge(std::shared_ptr<asio::io_service> aio, std::shared_ptr<tcp::socket> sock, std::shared_ptr<asio::signal_set> sigs, std::shared_ptr<void> semaphore)
			 : aio(move(aio)),
			   sock(move(sock)),
			   buf(std::make_shared<std::vector<char>>()),
			   sigs(move(sigs)),
			   received(),
			   semaphore(move(semaphore))
		{
		}
		compiler_bridge(const compiler_bridge &) = default;
		compiler_bridge &operator =(const compiler_bridge &) = default;
		compiler_bridge(compiler_bridge &&) = default;
		compiler_bridge &operator =(compiler_bridge &&) = default;
		void operator ()(error_code ec = error_code(), size_t len = 0) {
			reenter (this) while (true) {
				yield {
					const auto offset = buf->size();
					buf->resize(offset + BUFSIZ);
					PROTECT_FROM_MOVE(buf);
					PROTECT_FROM_MOVE(sock);
					sock->async_read_some(asio::buffer(asio::buffer(*buf) + offset), move(*this));
				}
				if (ec) return (void)sock->close(ec);
				buf->erase(buf->end()-(BUFSIZ-len), buf->end());

				auto ite = buf->begin();
				while (true) {
					std::string command;
					int len = 0;
					std::string data;
					if (!qi::parse(ite, buf->end(), +(qi::char_ - qi::space) >> qi::omit[*qi::space] >> qi::omit[qi::int_[phx::ref(len) = qi::_1]] >> qi::omit[':'] >> qi::repeat(phx::ref(len))[qi::char_] >> qi::omit[qi::eol], command, data)) break;
					if (command == "Control" && data == "run") {
						std::string ccname;
						const auto &s = received["Control"];
						{
							auto ite = s.begin();
							qi::parse(ite, s.end(), "compiler=" >> *qi::char_, ccname);
						}
						const auto c = config.compilers.get<1>().find(ccname);
						if (c == config.compilers.get<1>().end()) {
							std::clog << "[" << sock.get() << "]" << "selected compiler '" << ccname << "' is not configured" << std::endl;
							return (void)sock->close(ec);
						}
						return program_writer(move(aio), move(sock), move(sigs), move(received), move(sources), *c, move(semaphore))();
					} else if (command == "Version") {
						return version_sender(move(aio), move(sock), move(sigs), move(semaphore))();
					} else if (command == "SourceFileName") {
						current_filename = quoted_printable::decode(move(data));
					} else if (command == "Source") {
						sources[current_filename] += quoted_printable::decode(move(data));
					} else {
						received[command] += quoted_printable::decode(move(data));
					}
				}
				buf->erase(buf->begin(), ite);
			}
		}
		std::shared_ptr<asio::io_service> aio;
		std::shared_ptr<tcp::socket> sock;
		std::shared_ptr<std::vector<char>> buf;
		std::shared_ptr<asio::signal_set> sigs;
		std::unordered_map<std::string, std::string> received;
		std::unordered_map<std::string, std::string> sources;
		std::string current_filename;
		std::shared_ptr<void> semaphore;
	};

	struct listener: private coroutine {
		typedef void result_type;
		void operator ()(error_code = error_code()) {
			reenter (this) while (true) {
				sock = std::make_shared<tcp::socket>(*aio);
				yield {
					PROTECT_FROM_MOVE(sock);
					PROTECT_FROM_MOVE(acc);
					acc->async_accept(*sock, move(*this));
				}
				std::clog << "[" << sock.get() << "]" << "connection established from " << sock->remote_endpoint() << std::endl;
				yield compiler_bridge(aio, move(sock), sigs, sem->async_signal(*this))();
			}
		}
		template <typename ...Args>
		listener(std::shared_ptr<asio::io_service> aio, Args &&...args)
			 : aio(move(aio)),
			   ep(std::forward<Args>(args)...),
			   acc(std::make_shared<tcp::acceptor>(*this->aio, this->ep)),
			   sigs(std::make_shared<asio::signal_set>(*this->aio, SIGCHLD, SIGHUP)),
			   sock(),
			   sem(std::make_shared<counting_semaphore>(*this->aio, config.system.max_connections-1))
		{
			std::clog << "start listening at " << this->ep << std::endl;
			try {
				mkdir(config.system.basedir, 0700);
			} catch (std::system_error &e) {
				if (e.code().value() != EEXIST) {
					std::clog << "failed to create basedir, check permission." << std::endl;
					throw;
				}
			}
			basedir = opendir(config.system.basedir);
			chdir(basedir);
		}
		listener(const listener &) = default;
		listener &operator =(const listener &) = default;
		listener(listener &&) = default;
		listener &operator =(listener &&) = default;
	private:
		std::shared_ptr<asio::io_service> aio;
		tcp::endpoint ep;
		std::shared_ptr<tcp::acceptor> acc;
		std::shared_ptr<asio::signal_set> sigs;
		std::shared_ptr<DIR> basedir;
		std::shared_ptr<tcp::socket> sock;
		std::shared_ptr<counting_semaphore> sem;
	};

}
int main(int argc, char **argv) try {
	using namespace wandbox;

	::setlocale(LC_ALL, "C");

	std::shared_ptr<std::streambuf> logbuf(std::clog.rdbuf(), [](void*){});

	{
		namespace po = boost::program_options;

		std::vector<std::string> config_files{std::string(SYSCONFDIR) + "/cattleshed.conf", std::string(SYSCONFDIR) + "/cattleshed.conf.d"};
		{
			po::options_description opt("options");
			opt.add_options()
				("help,h", "show this help")
				("config,c", po::value<std::vector<std::string>>(&config_files), "specify config file")
				("syslog", "use syslog for trace")
				("verbose", "be verbose")
			;

			po::variables_map vm;
			po::store(po::parse_command_line(argc, argv, opt), vm);
			po::notify(vm);

			if (vm.count("help")) {
				std::cout << opt << std::endl;
				return 0;
			}

			if (vm.count("syslog")) {
				logbuf.reset(new syslogstreambuf("cattleshed", LOG_PID, LOG_DAEMON, LOG_DEBUG));
				std::clog.rdbuf(logbuf.get());
			}
		}
		try {
			config = load_config(config_files);
		} catch (...) {
			std::clog << "failed to read config file(s), check existence or syntax." << std::endl;
			throw;
		}

		if (config.system.storedir[0] != '/') {
			std::clog << "storedir must be absolute." << std::endl;
			return 1;
		}

		if (config.system.basedir[0] != '/') {
			std::clog << "basedir must be absolute." << std::endl;
			return 1;
		}
	}
	auto aio = std::make_shared<asio::io_service>();
	listener s(aio, boost::asio::ip::tcp::v4(), config.system.listen_port);
	s();
	aio->run();
	return 0;
} catch (std::exception &e) {
	std::clog << "fatal: " << e.what() << std::endl;
	return -1;
}
