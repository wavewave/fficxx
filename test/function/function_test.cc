#include <functional>
#include <iostream>

void f( void ) {

    std::cout << "f is called!" << std::endl;

}

void g( void ) {

    std::cout << "g is called!" << std::endl;

}


int main( int argc, char** argv ) {

    std::cout << "std::function test" << std::endl;

    std::function<void()> fptr (f);

    std::function<void()> fptr2 ;

    fptr2 = g;

    fptr();

    fptr2();

}
