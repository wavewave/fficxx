#include <vector>
#include "Vector.h"
#include "Foo.h"

#include "STLType.h"


// this was in STL/csrc/STLFoo.cpp
template<class ToType, class FromType>
ToType* to_nonconst(FromType* x) {
  return reinterpret_cast<ToType*>(x);
}


Vector_instance_s(int)
Vector_instance(Foo)

