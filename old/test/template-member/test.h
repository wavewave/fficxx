#include <iostream>
#include <memory>

class A {
public:
    A() {}
    ~A() {}

    template<typename T> void method( T* x );

    template<typename T> void method2( std::unique_ptr<T> x );
};

template<typename T> void A::method( T* x ) {
    std::cout << "in A::method" << std::endl;
    x->print();
}

template<typename T> void A::method2( std::unique_ptr<T> x ) {
    std::cout << "in A::method2" << std::endl;
    T* y = x.get();
    y->print();
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
