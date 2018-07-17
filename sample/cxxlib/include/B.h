#ifndef __B__ 
#define __B__

#include "A.h"
class B : public A 
{ 
public: 
  B();
  virtual void Foo(void); 
  virtual void Bar(void);   
}; 

#endif // __B__
