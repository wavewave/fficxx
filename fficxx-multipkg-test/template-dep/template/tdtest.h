#pragma once

#include <iostream>
#include <typeinfo>

template<typename P> class T1 {
private:
public:
  T1() {}
  void method() {
    std::cout << "In T1::method(), typeid(P) = " << typeid(P).name() << std::endl;
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
