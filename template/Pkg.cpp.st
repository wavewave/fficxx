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

#define CHECKPROTECT(x,y) IS_PAREN(IS_ ## x ## _ ## y ## _PROTECTED)

#define TYPECASTMETHOD(cname,mname,oname) \\
  IIF( CHECKPROTECT(cname,mname) ) ( \\
  (to_nonconst<oname,cname ## _t>), \\
  (to_nonconst<cname,cname ## _t>) )

$cppbody$

void dummy$modname$ ( void ) 
{
  
}

