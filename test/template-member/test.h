class A {
public:
    A() {}
    ~A() {}

    template<typename T> void method( T* x );
};

template<typename T> void A::method( T* x ) {
    x->print();
}


class T1 {
public:
    T1() {}
    ~T1() {}

    void print();
};
