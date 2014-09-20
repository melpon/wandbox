#include <cppcms/view.h>
#include <string>

namespace content {
    struct root : public cppcms::base_content {
        std::string title;
    };
}
