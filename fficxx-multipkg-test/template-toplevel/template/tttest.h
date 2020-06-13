#pragma once

#include <iostream>
#include <utility>
#include <vector>

void ordinary( void );

template<typename T>
std::vector<T> return_vector( int n ) {
  std::vector<T> v;
  std::cout << "template function is called" << std::endl;
  return std::move(v);
}
