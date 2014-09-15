#include <cppcms/view.h>  
#include <string>  

namespace content  {  
    struct message : public cppcms::base_content {  
        std::string text;  
    };  
}  
