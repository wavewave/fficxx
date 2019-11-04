#include <iostream>
#include "A.h"


A::A() { } 

void A::Foo() 
{ 
  std::cout << "A:Foo" << std::endl;  
} 

void A::Foo2( signed long t )
{
  std::cout << "A:Foo2 : got " << t << std::endl; 
}
 
