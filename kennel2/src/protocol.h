#ifndef PROTOCOL_H_INCLUDED
#define PROTOCOL_H_INCLUDED

#include <string>
#include <vector>
#include <cstdio>
#include "libs.h"
#include "quoted_printable.h"

struct protocol {
    std::string command;
    std::string contents;

    std::string to_string() const {
        auto qp = quoted_printable::encode(contents);
        return command + " " + std::to_string(qp.size()) + ":" + qp + "\n";
    }
};

class async_read_protocol_t : public booster::enable_shared_from_this<async_read_protocol_t> {
    enum class read_state_t {
        command,
        size,
        contents,
    };
    read_state_t state;
    int content_size;
    std::string command;
    std::string contents;

    enum class consume_state_t {
        more,
        read_line,
        read_completed,
        error_large_command,
        error_large_content_size,
        error_out_of_digit,
        error_invalid_content_size,
    };

    consume_state_t consume(char c) {
        if (state == read_state_t::command) {
            if (c == '\0') {
                return consume_state_t::read_completed;
            }
            if (c == ' ') {
                state = read_state_t::size;
            } else {
                if (command.size() >= 128) {
                    return consume_state_t::error_large_command;
                }
                command.push_back(c);
            }
        } else if (state == read_state_t::size) {
            if (c == ':') {
                state = read_state_t::contents;
            } else if ('0' <= c && c <= '9') {
                if (content_size >= 64 * 1024 * 1024) {
                    return consume_state_t::error_large_content_size;
                }
                content_size = content_size * 10 + (c - '0');
            } else {
                return consume_state_t::error_out_of_digit;
            }
        } else if (state == read_state_t::contents) {
            if (content_size == static_cast<int>(contents.size())) {
                if (c == '\n') {
                    return consume_state_t::read_line;
                } else {
                    return consume_state_t::error_invalid_content_size;
                }
            }
            contents.push_back(c);
        }
        return consume_state_t::more;
    }
    void clear() {
        state = read_state_t::command;
        content_size = 0;
        command.clear();
        contents.clear();
    }

    void disconnect() {
        clear();
        if (sock) {
            booster::system::error_code ec;
            sock->shutdown(booster::aio::stream_socket::shut_rdwr, ec);
            sock.reset();
        }
    }
public:
    typedef booster::function<void (const booster::system::error_code&, const protocol&)> handler_t;
    typedef booster::shared_ptr<booster::aio::stream_socket> socket_ptr_t;

private:
    socket_ptr_t sock;
    handler_t handler;
    char buf[BUFSIZ];
    int line;
    int max_line;

    bool read_(const booster::system::error_code& e, std::size_t size) {
        if (e) {
            (void)handler(e, protocol());
            disconnect();
            return true;
        }
        for (std::size_t i = 0; i < size; i++) {
            auto c = buf[i];
            auto cs = consume(c);
            if (cs == consume_state_t::more) {
                continue;
            }

            if (cs == consume_state_t::read_line) {
                protocol proto;
                proto.command = command;
                proto.contents = quoted_printable::decode(contents);
                handler(e, proto);
                line += 1;
                if ((command == "Control" && contents == "Finish") ||
                    (max_line > 0 && line == max_line)) {
                    disconnect();
                    return true;
                } else {
                    clear();
                }
            }
            if (cs == consume_state_t::read_completed) {
                disconnect();
                return true;
            }
            if (cs == consume_state_t::error_large_command ||
                cs == consume_state_t::error_large_content_size ||
                cs == consume_state_t::error_out_of_digit ||
                cs == consume_state_t::error_invalid_content_size) {
                // error
                booster::system::error_code ec(-1, booster::system::system_category);
                handler(ec, protocol());
                disconnect();
                return true;
            }
        }
        return false;
    }

public:
    async_read_protocol_t(socket_ptr_t sock, const handler_t& handler, int max_line = 0) : sock(sock), handler(handler), line(0), max_line(max_line) {
        clear();
    }

    void read() {
        while (true) {
            booster::system::error_code ec;
            auto size = sock->read_some(booster::aio::buffer(buf, sizeof(buf)), ec);
            if (this->read_(ec, size)) {
                break;
            }
        }
    }
    void read_async() {
        auto self = this->shared_from_this();
        sock->async_read_some(
            booster::aio::buffer(buf, sizeof(buf)),
            [self](const booster::system::error_code& e, std::size_t size) {
                if (!self->read_(e, size))
                    self->read_async();
            });
    }
};

template<class F>
void send_command(booster::aio::io_service& service, booster::aio::endpoint ep, std::vector<protocol> protos, F f, int max_line = 0) {
    booster::shared_ptr<booster::aio::stream_socket> sock(new booster::aio::stream_socket(service));

    std::clog << '[' << sock.get() << ']' << "open start" << std::endl;
    booster::system::error_code ec;
    sock->open(booster::aio::family_type::pf_inet);

    std::clog << '[' << sock.get() << ']' << "connect start" << std::endl;
    sock->connect(ep);

    std::clog << '[' << sock.get() << ']' << "connected" << std::endl;
    std::string send_string;
    for (auto&& proto: protos) {
        send_string += proto.to_string();
    }

    sock->write(booster::aio::buffer(send_string));

    booster::shared_ptr<async_read_protocol_t> arp(new async_read_protocol_t(sock, f, max_line));
    arp->read();
    std::clog << '[' << sock.get() << ']' << "finish" << std::endl;
}

template<class F>
void send_command_async(booster::aio::io_service& service, booster::aio::endpoint ep, std::vector<protocol> protos, F f, int max_line = 0) {
    booster::shared_ptr<booster::aio::stream_socket> sock(new booster::aio::stream_socket(service));

    std::clog << '[' << sock.get() << ']' << "open start" << std::endl;
    booster::system::error_code ec;
    sock->open(booster::aio::family_type::pf_inet, ec);
    if (ec)
        return (void)f(ec, protocol());

    std::clog << '[' << sock.get() << ']' << "connect start" << std::endl;
    sock->async_connect(ep, [sock, f, max_line, protos](const booster::system::error_code& e) {
        if (e)
            return (void)f(e, protocol());
        std::clog << '[' << sock.get() << ']' << "connected" << std::endl;
        std::string send_string;
        for (auto&& proto: protos) {
            send_string += proto.to_string();
        }
        std::size_t send_string_size = send_string.size();

        sock->async_write(booster::aio::buffer(send_string), [sock, f, max_line, send_string_size](const booster::system::error_code& e, std::size_t send_size) {
            if (e)
                return (void)f(e, protocol());
            assert(send_size == send_string_size);

            std::clog << '[' << sock.get() << ']' << "written" << std::endl;

            booster::shared_ptr<async_read_protocol_t> arp(new async_read_protocol_t(sock, f, max_line));
            arp->read_async();
            std::clog << '[' << sock.get() << ']' << "finish" << std::endl;
        });
    });
}

template<class F>
void send_command(cppcms::service& srv, std::size_t n, std::vector<protocol> protos, F f, int max_line = 0) {
    auto host = srv.settings()["application"]["cattleshed"][n]["host"].str();
    auto port = (int)srv.settings()["application"]["cattleshed"][n]["port"].number();
    booster::aio::endpoint ep(host, port);

    send_command(srv.get_io_service(), ep, protos, f, max_line);
}

template<class F>
void send_command_async(cppcms::service& srv, std::size_t n, std::vector<protocol> protos, F f, int max_line = 0) {
    auto host = srv.settings()["application"]["cattleshed"][n]["host"].str();
    auto port = (int)srv.settings()["application"]["cattleshed"][n]["port"].number();
    booster::aio::endpoint ep(host, port);

    send_command_async(srv.get_io_service(), ep, protos, f, max_line);
}

#endif // PROTOCOL_H_INCLUDED
