#include <MacroPatternMatch.h>
#include <memory>
#include <string>
#include "UniquePtr.h"
#include "stdcxxType.h"

using namespace std;

//UniquePtr_instance(string)

/*
extern "C" { void* UniquePtr_new_string ( string* p ); }
inline void* UniquePtr_new_string ( string* p ) { return static_cast<void*>(new std::unique_ptr<string>(p)); }
auto a_UniquePtr_new_string = UniquePtr_new_string ;
*/

// extern "C" { string_p UniquePtr_get_string ( void* p ); } inline string_p UniquePtr_get_string ( void* p ) { return to_nonconst<string_t, string>((string *)&((static_cast<std::unique_ptr<string>*>(p))->get())) ; } auto a_UniquePtr_get_string = UniquePtr_get_string ; extern "C" { string_p UniquePtr_release_string ( void* p ); } inline string_p UniquePtr_release_string ( void* p ) { return to_nonconst<string_t, string>((string *)&((static_cast<std::unique_ptr<string>*>(p))->release())) ; } auto a_UniquePtr_release_string = UniquePtr_release_string ; extern "C" { void UniquePtr_reset_string ( void* p ); } inline void UniquePtr_reset_string ( void* p ) { (static_cast<std::unique_ptr<string>*>(p))->reset(); } auto a_UniquePtr_reset_string = UniquePtr_reset_string ; extern "C" { void UniquePtr_delete_string ( void* p ); } inline void UniquePtr_delete_string ( void* p ) { delete (static_cast<std::unique_ptr<string>*>(p)); } auto a_UniquePtr_delete_string = UniquePtr_delete_string ;


