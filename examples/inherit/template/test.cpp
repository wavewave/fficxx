#include "test.h"
#include <iostream>

void Impl::action() {
  std::cout << "Impl::action()" << std::endl;
}

void ImplSub::action() {
  std::cout << "ImplSub::action()" << std::endl;
}
