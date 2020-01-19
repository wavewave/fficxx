#pragma once

#include <iostream>

template<typename P> class T1 {
private:
  //P& p;
public:
  T1() {}
  //T1(P& f) : p(f) {}
  void method() {
    std::cout << "In T1::method(), " << std::endl;
  }

};

template<typename P> class T2 {
private:
public:
  T2() {}
  void callT1( T1<P>& tmpl1 ) {
    std::cout << "In T2::callT1(), calling T1::method: " << std::endl;
    tmpl1.method();
  }

};
