#include <map>
#include <string>
#include <vector>
#include <thread>
#include <sstream>
#include <functional>
#include <list>
#include <fstream>
#include <boost/range.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/range/istream_range.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/program_options.hpp>
#include <boost/system/system_error.hpp>

#include "common.hpp"
#include "load_config.hpp"
#include "posixapi.hpp"

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

	template <typename Stream>
	struct stream_pair {
		template <typename ...Args>
		stream_pair(asio::io_service &aio, Args &&...args): stream(aio, std::forward<Args>(args)...), buf() { }
		Stream stream;
		asio::streambuf buf;
	};
	typedef stream_pair<asio::posix::stream_descriptor> stream_descriptor_pair;


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

	struct thread_compare {
		bool operator ()(const std::thread &x, const std::thread &y) const {
			return x.get_id() < y.get_id();
		}
	};

	struct compiler_bridge {
		bool is_any_of(const string& str, const string& value) const {
			vector<string> result;
			boost::algorithm::split(result, str, [](char c) { return c == ','; });
			return std::any_of(result.begin(), result.end(), [&](const string& v) { return v == value; });
		}
		const compiler_trait &get_compiler() const {
			const auto compiler = [this]() -> string {
				string ret;
				const auto &s = received.at("Control");
				auto ite = s.begin();
				qi::parse(ite, s.end(), "compiler=" >> *qi::char_, ret);
				return ret;
			}();
		    return *config.compilers.get<1>().find(compiler);
		}
		vector<string> get_compiler_arg() const {
			const auto &c = get_compiler();
			vector<string> args = c.compile_command;
			const auto it = received.find("CompilerOption");
			if (it != received.end()) {
				for (const auto &sw: c.switches) {
					if (is_any_of(it->second, sw.name)) {
						args.insert(args.end(), sw.flags.begin(), sw.flags.end());
					}
				}
			}
			return args;
		}
		void send_version() {
			const auto proc = [this](const vector<string>& args) -> std::string {
				auto c = piped_spawn(_P_WAIT, opendir("/"), args);
				stream_descriptor_pair s(aio, c.fd_stdout.release());
				if (c.pid == -1 || !WIFEXITED(c.pid) || WEXITSTATUS(c.pid) != 0) return "";
				error_code ec;
				asio::read(s.stream, s.buf, ec);
				return std::string(asio::buffer_cast<const char *>(s.buf.data()), asio::buffer_size(s.buf.data()));
			};
			string line;
			for (const auto &c: config.compilers) {
				if (!c.displayable) continue;
				if (c.version_command.empty()) continue;
				string ver;
				{
					std::stringstream ss(proc(c.version_command));
					getline(ss, ver);
				}
				if (ver.empty()) continue;
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
			line = encode_qp(line);
			const auto str = ([&]() -> string {
				std::stringstream ss;
				ss << "VersionResult " << line.length() << ':' << line << '\n';
				return ss.str();
			})();
			aio.post([this, str] {
				error_code ec;
				asio::write(sock, asio::buffer(str), ec);
			});
			aio.run();
		}

		bool wait_run_command() {
			asio::streambuf buf;
			bool exhausted = false;
			while (true) {
				asio::read_until(sock, buf, end_read_condition(asio::buffer_size(buf.data()) + (exhausted ? 1 : 0)));
				const auto begin = asio::buffer_cast<const char *>(buf.data());
				auto ite = begin;
				const auto end = ite + asio::buffer_size(buf.data());

				std::string command;
				int len = 0;
				std::string data;
				if (qi::parse(ite, end, +(qi::char_ - qi::space) >> qi::omit[*qi::space] >> qi::omit[qi::int_[phx::ref(len) = qi::_1]] >> qi::omit[':'] >> qi::repeat(phx::ref(len))[qi::char_] >> qi::omit[qi::eol], command, data)) {

					if (command == "Control" && data == "run") return true;
					if (command == "Version") {
						send_version();
						return false;
					}
					received[command] += decode_qp(data);
					exhausted = false;
				} else {
					exhausted = true;
				}
				buf.consume(ite - begin);
			}
		}

		void operator ()() {
			// io thread

			if (!wait_run_command()) return;
			const string srcname = "prog" + get_compiler().source_suffix;

			auto &s = received["Source"];
			{
				unique_fd fd(::openat(::dirfd(workdir.get()), srcname.c_str(), O_WRONLY|O_CLOEXEC|O_CREAT|O_TRUNC, 0600));
				for (std::size_t offset = 0; offset < s.length(); ) {
					const auto wrote = ::write(fd.get(), s.data()+offset, s.length()-offset);
					if (wrote < 0 && errno != EINTR) throw_system_error(errno);
					offset += wrote;
				}
				::fsync(fd.get());
				::close(fd.get());
			}

			auto c = piped_spawn(_P_NOWAIT, workdir, get_compiler_arg());
			cc_pid = c.pid;

			aio.post([this] {
				error_code ec;
				const string str = "Control 5:Start\n";
				asio::write(sock, asio::buffer(str), ec);
			});

			pipes.emplace_front(ref(aio));
			auto cc_stdout = pipes.begin();
			cc_stdout->stream.assign(c.fd_stdout.release());
			asio::async_read_until(
				cc_stdout->stream, cc_stdout->buf, end_read_condition(),
				std::bind<void>(
					ref(*this),
					cc_stdout,
					cc_pid,
					"CompilerMessageS",
					_1,
					_2));

			pipes.emplace_front(ref(aio));
			auto cc_stderr = pipes.begin();
			cc_stderr->stream.assign(c.fd_stderr.release());
			asio::async_read_until(
				cc_stderr->stream, cc_stderr->buf, end_read_condition(),
				std::bind<void>(
					ref(*this),
					cc_stderr,
					cc_pid,
					"CompilerMessageE",
					_1,
					_2));

			timer.expires_from_now(ptime::seconds(config.jail.compile_time_limit));
			timer.async_wait(std::bind<void>(ref(*this), _1, cc_pid, SIGXCPU));

			aio.run();
		}

		void send_exitcode(int code) {
			aio.post([this,code] {
				error_code ec;
				string str;
				if (WIFSIGNALED(code)) {
					const char *sig = ::strsignal(WTERMSIG(code));
					if (not sig) sig = "";
					str += "Signal " + boost::lexical_cast<std::string>(::strlen(sig)) + ":" + sig + "\n";
				}
				if (WIFEXITED(code)) {
					const auto c = boost::lexical_cast<std::string>(WEXITSTATUS(code));
					str += "ExitCode " + boost::lexical_cast<std::string>(c.length()) + ":" + c + "\n";
				}
				str += "Control 6:Finish\n";
				asio::write(sock, asio::buffer(str), ec);
				sock.close();
				aio.post(std::bind<void>(&asio::io_service::stop, ref(aio)));
			});
		}

		void check_finish() {
			if (!pipes.empty()) return;
			if (prog_pid) {
				int st;
				waitpid(prog_pid, &st, 0);
				timer.cancel();
				send_exitcode(st);
			} else {
				int st;
				waitpid(cc_pid, &st, 0);
				timer.cancel();
				if (st) {
					prog_pid = -1;
					send_exitcode(st);
				} else {
					vector<string> runargs = get_compiler().run_command;
					runargs.insert(runargs.begin(), { ptracer, "--config=" + config_file, "--" });
					auto c = piped_spawn(_P_NOWAIT, workdir, runargs);
					prog_pid = c.pid;

					pipes.emplace_front(ref(aio));
					auto aout_stdout = pipes.begin();
					aout_stdout->stream.assign(c.fd_stdout.release());
					asio::async_read_until(
						aout_stdout->stream, aout_stdout->buf, end_read_condition(),
						std::bind<void>(
							ref(*this),
							aout_stdout,
							prog_pid,
							"StdOut",
							_1,
							_2));

					pipes.emplace_front(ref(aio));
					auto aout_stderr = pipes.begin();
					aout_stderr->stream.assign(c.fd_stderr.release());
					asio::async_read_until(
						aout_stderr->stream, aout_stderr->buf, end_read_condition(),
						std::bind<void>(
							ref(*this),
							aout_stderr,
							prog_pid,
							"StdErr",
							_1,
							_2));

					timer.expires_from_now(ptime::seconds(config.jail.program_duration));
					timer.async_wait(std::bind<void>(ref(*this), _1, prog_pid, SIGXCPU));
				}
			}
		}

		void operator ()(error_code ec, pid_t pid, int sig) {
			if (ec) return;
			::kill(pid, sig);
			if (sig != SIGKILL) {
				timer.expires_from_now(ptime::seconds(config.jail.kill_wait));
				timer.async_wait(std::bind<void>(ref(*this), _1, pid, SIGKILL));
			}
		}

		void operator ()(asio::streambuf &, error_code, size_t) {
		}

		void operator ()(std::list<stream_descriptor_pair>::iterator p, pid_t pid, const char *msg, error_code ec, size_t) {
			auto &pipe = p->stream;
			auto &rbuf = p->buf;
			std::vector<char> data(BUFSIZ);
			data.resize(asio::buffer_copy(asio::buffer(data), rbuf.data()));
			rbuf.consume(data.size());
			const auto str = ([&]() -> string {
				const auto line = encode_qp(data);
				std::stringstream ss;
				ss << msg << ' ' << line.length() << ':' << line << '\n';
				return ss.str();
			})();
			aio.post([this, str] {
				error_code ec;
				asio::write(sock, asio::buffer(str), ec);
			});
			if (data.empty() || ec) {
				pipes.erase(p);
				check_finish();
			} else {
				asio::async_read_until(pipe, rbuf, end_read_condition(), std::bind<void>(ref(*this), p, pid, msg, _1, _2));
			}
			output_size += data.size();
			if (output_size > config.jail.output_limit_kill) ::kill(pid, SIGKILL);
			else if (output_size > config.jail.output_limit_warn) ::kill(pid, SIGXFSZ);
		}

		compiler_bridge(tcp::acceptor &acc): aio(), sock(aio), timer(aio), workdir(), pipes(), received(), cc_pid(), prog_pid(), output_size()
		{
			acc.accept(sock);
			do {
				try {
					workdir = opendir(mkdtemp("wandboxXXXXXX"));
				} catch (std::system_error &e) {
					if (e.code().value() != ENOTDIR) throw;
				}
			} while (!workdir && errno == ENOTDIR);
			cc_pid = 0;
			prog_pid = 0;
		}
	private:
		asio::io_service aio;
		tcp::socket sock;
		asio::deadline_timer timer;
		shared_ptr<DIR> workdir;
		list<stream_descriptor_pair> pipes;
		std::map<std::string, std::string> received;
		int cc_pid, prog_pid;
		std::size_t output_size;
	};

	struct listener {
		template <typename ...Args>
		void operator ()(Args &&...args) {
			asio::io_service aio;
			tcp::endpoint ep(std::forward<Args>(args)...);
			tcp::acceptor acc(aio, ep);
			while (1) {
				shared_ptr<compiler_bridge> pcb(make_shared<compiler_bridge>(ref(acc)));
				std::thread([pcb] { (*pcb)(); }).detach();
			}
		}
		listener() {
			try {
				mkdir(config.jail.basedir, 0700);
			} catch (std::system_error &e) {
				if (e.code().value() != EEXIST) throw;
			}
			basedir = opendir(config.jail.basedir);
			chdir(basedir);
		}
	private:
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
	listener s;
	s(boost::asio::ip::tcp::v4(), config.network.listen_port);
}
