extern "C" {
  void* Function_new_f1 ( void (*fp) ( ));
  void Function_call_f1 ( void* );
  void Function_delete_f1 ( void* );
}

inline void* Function_new_f1 ( void (*fp)( ) ) {
  std::function< void ( )>* p = new std::function< void ( ) >(fp);
  return static_cast<void*>(p);
}
auto a_Function_new_f1 = Function_new_f1;

inline void Function_call_f1 ( void* op ) {
  std::function< void ( )>* p = static_cast< std::function< void ( )>* >(op);
  return (*p) ( );
}
auto a_Function_call_f1 = Function_call_f1;

inline void Function_delete_f1 ( void* op ) {
  std::function< void ( ) >* p = static_cast< std::function< void ( ) >* >(op);
  delete p;
}
auto a_Function_delete_f1 = Function_delete_f1;

extern "C" {
  void* Function_new_f2 ( void (*fp) ( int ));
  void Function_call_f2 ( void* , int );
  void Function_delete_f2 ( void* );
}

inline void* Function_new_f2 ( void (*fp)( int ) ) {
  std::function< void ( int )>* p = new std::function< void ( int ) >(fp);
  return static_cast<void*>(p);
}
auto a_Function_new_f2 = Function_new_f2;

inline void Function_call_f2 ( void* op , int x ) {
  std::function< void ( int )>* p = static_cast< std::function< void ( int )>* >(op);
  return (*p) ( x );
}
auto a_Function_call_f2 = Function_call_f2;

inline void Function_delete_f2 ( void* op ) {
  std::function< void ( int ) >* p = static_cast< std::function< void ( int ) >* >(op);
  delete p;
}
auto a_Function_delete_f2 = Function_delete_f2;

extern "C" {
  void* Function_new_f3 ( int (*fp) ( int ));
  int Function_call_f3 ( void* , int );
  void Function_delete_f3 ( void* );
}
inline void* Function_new_f3 ( int (*fp)( int ) ) {
  std::function< int ( int )>* p = new std::function< int ( int ) >(fp);
  return static_cast<void*>(p);
}
auto a_Function_new_f3 = Function_new_f3;

inline int Function_call_f3 ( void* op , int x ) {
  std::function< int ( int )>* p = static_cast< std::function< int ( int )>* >(op);
  return (*p) ( x );
}
auto a_Function_call_f3 = Function_call_f3;

inline void Function_delete_f3 ( void* op ) {
  std::function< int ( int ) >* p = static_cast< std::function< int ( int ) >* >(op);
  delete p;
}
auto a_Function_delete_f3 = Function_delete_f3;

extern "C" {
  void* Function_new_f4 ( int (*fp) ( int , char ));
  int Function_call_f4 ( void* , int , char );
  void Function_delete_f4 ( void* );
}
inline void* Function_new_f4 ( int (*fp)( int , char ) ) {
  std::function< int ( int , char )>* p = new std::function< int ( int , char ) >(fp);
  return static_cast<void*>(p);
}
auto a_Function_new_f4 = Function_new_f4;

inline int Function_call_f4 ( void* op , int x , char y ) {
  std::function< int ( int , char )>* p = static_cast< std::function< int ( int , char )>* >(op);
  return (*p) ( x , y );
}
auto a_Function_call_f4 = Function_call_f4;

inline void Function_delete_f4 ( void* op ) {
  std::function< int ( int , char ) >* p = static_cast< std::function< int ( int , char ) >* >(op);
  delete p;
}
auto a_Function_delete_f4 = Function_delete_f4;

extern "C" {
  void* Function_new_f5 ( int (*fp) ( ));
  int Function_call_f5 ( void* );
  void Function_delete_f5 ( void* );
}

inline void* Function_new_f5 ( int (*fp)( ) ) {
  std::function< int ( )>* p = new std::function< int ( ) >(fp);
  return static_cast<void*>(p);
}
auto a_Function_new_f5 = Function_new_f5;

inline int Function_call_f5 ( void* op ) {
  std::function< int ( )>* p = static_cast< std::function< int ( )>* >(op);
  return (*p) ( );
}
auto a_Function_call_f5 = Function_call_f5;

inline void Function_delete_f5 ( void* op ) {
  std::function< int ( ) >* p = static_cast< std::function< int ( ) >* >(op); delete p;
}
auto a_Function_delete_f5 = Function_delete_f5;
