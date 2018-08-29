#include <stdio.h>
#include "function_test.h"

void f1() {
    printf("in f1\n");
}

void f2(int x) {
    printf("in f2\nx = %d\n",x);
}

int f3(int x) {
    printf("in f3\nx = %d\n",x);
    return (x+1);
}

int f4(int x, char y) {
    printf("in f4\nx = %d, y = %c\n",x,y);
    return (x+1);
}


int f5() {
    printf("in f5\n");
    return 3;
}

void test1() {

    printf("----\n");
    void* f1p = newFunction1(&f1);
    callFunction1(f1p);

    printf("----\n");
    void* f2p = newFunction2(&f2);
    callFunction2(f2p,30);

    printf("----\n");
    void* f3p = newFunction3(&f3);
    int r3 = callFunction3(f3p,10);
    printf("result = %d\n",r3);

    printf("----\n");
    void* f4p = newFunction4(&f4);
    int r4 = callFunction4(f4p,10,'k');
    printf("result = %d\n",r4);
}


// using macro.
void test2() {

    printf("----\n");
    void* f1p = Function_new_f1(&f1);
    Function_call_f1(f1p);

    printf("----\n");
    void* f2p = Function_new_f2(&f2);
    Function_call_f2(f2p,30);

    printf("----\n");
    void* f3p = Function_new_f3(&f3);
    int r3 = Function_call_f3(f3p,10);
    printf("result = %d\n",r3);

    printf("----\n");
    void* f4p = Function_new_f4(&f4);
    int r4 = Function_call_f4(f4p,10,'k');
    printf("result = %d\n",r4);


    printf("----\n");
    void* f5p = Function_new_f5(&f5);
    int r5 = Function_call_f5(f5p);
    printf("result = %d\n",r5);

}

int main( int argc, char** argv ) {
    //test1();
    test2();
    return 0;
}
