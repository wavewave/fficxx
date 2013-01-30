#include <MacroPatternMatch.h>

$header$

using namespace std;
$namespace$

template<class ToType, class FromType>
const ToType* to_const(const FromType* x) {
  return reinterpret_cast<const ToType*>(x);
}

template<class ToType, class FromType>
ToType* to_nonconst(FromType* x) {
  return reinterpret_cast<ToType*>(x);
}



$cppbody$

void dummy$modname$ ( void ) 
{
  
}

