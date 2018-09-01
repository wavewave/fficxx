#include "test.h"

int main( int argc, char** argv ) {

    A* a;
    T1* t1;
    a = new A();
    t1 = new T1();

    a-> method<T1>( t1 );

}
