#include <functional>
#include <iostream>
#include "function_test.h"


void* newFunction1( void (*fp)() ) {
    std::function<void()>* p = new std::function<void()>(fp);
    return static_cast<void*>(p);
}

void callFunction1( void* stdfn ) {
    std::function<void()>* p = static_cast<std::function<void()>*>(stdfn);
    (*p)();
}

void* newFunction2( void (*fp)(int) ) {
    std::function<void(int)>* p = new std::function<void(int)>(fp);
    return static_cast<void*>(p);
}

void callFunction2( void* stdfn, int x ) {
    std::function<void(int)>* p = static_cast<std::function<void(int)>*>(stdfn);
    (*p)(x);
}


void* newFunction3( int (*fp)(int) ) {
    std::function<int(int)>* p = new std::function<int(int)>(fp);
    return static_cast<void*>(p);
}

int callFunction3( void* stdfn, int x ) {
    std::function<int(int)>* p = static_cast<std::function<int(int)>*>(stdfn);
    return (*p)(x);
}

void* newFunction4( int (*fp)(int,char) ) {
    std::function<int(int,char)>* p = new std::function<int(int,char)>(fp);
    return static_cast<void*>(p);
}

int callFunction4( void* stdfn, int x, char y ) {
    std::function<int(int,char)>* p = static_cast<std::function<int(int,char)>*>(stdfn);
    return (*p)(x,y);
}
