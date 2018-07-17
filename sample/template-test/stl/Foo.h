#ifndef __FOO__
#define __FOO__ 

#include <iostream>

using namespace std;

class Foo
{
 private:
  int n;
 public: 
  Foo(int m) { n = m; }; 
  virtual void showme( ) { cout << "Foo: " << n << endl; }
  
}; 

#endif // __FOO__
