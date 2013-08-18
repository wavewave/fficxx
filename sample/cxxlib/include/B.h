#ifndef __B__ 
#define __B__

#include "A.h"
class B : public A 
{ 
public: 
  B();
  virtual void Foo(void); 
  virtual void Bar(void);   
protected:
  int Bar2();
  char Bar3;
}; 

#endif // __B__
