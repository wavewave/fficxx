#include <iostream>

class A {
public:
    A() {}
    ~A() {}

    template<typename T> void method( T* x );
};

template<typename T> void A::method( T* x ) {
    std::cout << "in A::method" << std::endl;
    x->print();
}


class T1 {
public:
    T1() {}
    ~T1() {}

    void print();
};

class T2 {
public:
    T2() {}
    ~T2() {}

    void print();
};
