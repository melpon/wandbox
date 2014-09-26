#ifndef QUOTED_PRINTABLE_H_INCLUDED
#define QUOTED_PRINTABLE_H_INCLUDED

#include <utility>

class quoted_printable {
    static std::pair<char, char> to_hex(char c) {
        const char tbl[] = "0123456789ABCDEF";
        return std::make_pair(tbl[(unsigned char)c >> 4], tbl[c & 0xF]);
    }
    static int from_hex(char n) {
        if ('0' <= n && n <= '9') {
            return (int)(n - '0');
        }
        if ('A' <= n && n <= 'F') {
            return (int)(n - 'A') + 10;
        }
        if ('a' <= n && n <= 'f') {
            return (int)(n - 'a') + 10;
        }
        throw 0;
    }
    static char from_hex(char a, char b) {
        return (char)((from_hex(a) << 4) + from_hex(b));
    }

    static bool is_encode(char c) {
        auto w = (unsigned char)c;
        return w < 33 || w == 61 || w > 126;
    }

public:
    static std::string encode(const std::string& str) {
        std::string r;
        r.reserve(str.size() + str.size() / 2);
        int n = 0;
        for (auto&& c: str) {
            bool enc = is_encode(c);
            if (enc) {
                auto h = to_hex(c);
                if (n >= 73) {
                    r += "=\n=";
                    r.push_back(h.first);
                    r.push_back(h.second);
                    n = 3;
                } else {
                    r.push_back('=');
                    r.push_back(h.first);
                    r.push_back(h.second);
                    n += 3;
                }
            } else {
                if (n >= 75) {
                    r += "=\n";
                    r.push_back(c);
                    n = 1;
                } else {
                    r.push_back(c);
                    n += 1;
                }
            }
        }
        return r;
    }

    static std::string decode(const std::string& str) {
        std::string r;
        r.reserve(str.size());
        std::size_t n = 0;
        while (true) {
            if (n == str.size())
                break;
            auto c = str[n++];
            if (c == '=') {
                if (n == str.size()) {
                    throw 0;
                }
                auto c2 = str[n++];
                if (c2 == '\n') {
                    // through
                } else {
                    if (n == str.size()) {
                        throw 0;
                    }
                    auto c3 = str[n++];
                    r.push_back(from_hex(c2, c3));
                }
            } else {
                r.push_back(c);
            }
        }
        return r;
    }
};

#endif // QUOTED_PRINTABLE_H_INCLUDED
