#ifdef __cplusplus
extern "C" {
#endif

// testing void
void* newFunction1( void(*fp)() );

void callFunction1( void* stdfn );

// testing param
void* newFunction2( void(*fp)(int) );

void callFunction2( void* stdfn, int x );

// testing return
void* newFunction3( int(*fp)(int) );

int callFunction3( void* stdfn, int x );

// testing multiparam
void* newFunction4( int(*fp)(int,char) );

int callFunction4( void* stdfn, int x, char y);


#define Function_new(NAME)\
  void* Function_new_ ## NAME ( void(*fp)() );

#define Function_call(NAME)\
  void* Function_call_ ## NAME ( void* fp );


Function_new(f1)

Function_call(f1)

#ifdef __cplusplus
}
#endif
