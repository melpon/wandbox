// This file is a "Hello, world!" in C++ language by GCC for wandbox.
#include <iostream>

template<typename... Ts>
constexpr auto compare(Ts... args)
{
    auto ret{0};
    auto retVal = [&](const auto& val1){  auto lval = ((val1 > ret) ? ret : val1);
                                        if(val1 < ret)
                                            ret = val1;
                                            
                          std::cout<<"Value of lval is: "<<lval<<"\n";
                           return(lval);                                                  };
    
   (retVal(args), ...);   
    return ret;
}


int main()
{
    std::cout << "Hello, Wandbox!" << std::endl;
        
    std::cout<<"Comparisons compare"<<"\n";
        
    int a = -3;
    int b = -7;
    int z = 9;
    int s = -17;
    float q = 3.1346;
    float r = 2.6474;
    double x = 456.9;
    double o = 660.95;
    int p = -355345;
    int k = 5795;
    
    auto qxz = compare(a,b,z,s,q,r,x,o,p,k);
     
   std::cout<<"qxz is "<<qxz<<"\n"; 
}

// GCC reference:
//   https://gcc.gnu.org/

// C++ language references:
//   https://cppreference.com/
//   https://isocpp.org/
//   http://www.open-std.org/jtc1/sc22/wg21/

// Boost libraries references:
//   https://www.boost.org/doc/
