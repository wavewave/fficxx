#include <stdio.h>
#include "function_test.h"

void f() {
    printf("test function\n");
}

int main( int argc, char** argv ) {
    void* fp = newFunction(&f);
    callFunction(fp);
    return 0;
}
