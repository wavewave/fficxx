#ifndef __FOO__
#define __FOO__ 

#include <iostream>
#include <vector>

using namespace std;

class Foo
{
 private:
  int n;
  vector<int> t; 
 public: 
  Foo(int m) {
    n = m;
    t.push_back(101);
    t.push_back(102);
    t.push_back(103);
  }; 
  virtual void showme( ) { cout << "Foo: " << n << endl; }
  virtual vector<int>* getVector() { return &t ; }
  virtual void addContents( vector<int>* v ) {
    for( auto& x : *v ) {
      t.push_back( x ); 
    }
  }
  
}; 

#endif // __FOO__
