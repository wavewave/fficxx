#include "test.h"
#include <memory>

int main( int argc, char** argv ) {

    A* a;
    T1* t1;
    a = new A();
    t1 = new T1();

    a-> method<T1>( t1 );

    std::unique_ptr<T1> ptr(t1);

    a->method2<T1>(std::move(ptr));


}
