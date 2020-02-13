#pragma once

#include <utility>
#include <vector>

void ordinary( void );

template<typename T>
std::vector<T> return_vector( int n ) {
  std::vector<T> v;
  return std::move(v);
}
