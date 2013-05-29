#include <iostream>
#include "B.h"

B::B() { } 

void B::Foo( void ) 
{
  std::cout << "B:foo" << std::endl;  
}

void B::Bar( void ) 
{ 
  std::cout << "bar" << std::endl ; 
} 

