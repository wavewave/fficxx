#include <functional>
#include <iostream>
#include "function_test.h"


void* newFunction( void (*fp)() ) {
    std::function<void()>* p = new std::function<void()>(fp);
    return static_cast<void*>(p);
}

void callFunction( void* stdfn ) {
    std::function<void()>* p = static_cast<std::function<void()>*>(stdfn);
    (*p)();
}


/*
int main( int argc, char** argv ) {

    std::cout << "std::function test" << std::endl;

    std::function<void()> fptr (f);

    std::function<void()> fptr2 ;

    fptr2 = g;

    fptr();

    fptr2();

    std::function<void()> fptr3;

    bind_function(&fptr3,&g);
    fptr3();

}
*/
