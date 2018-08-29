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

#define VARGS(...) __VA_ARGS__

#define TAIL(x,...) __VA_ARGS__

#define UNPAREN(x) VARGS x


#define Function_new(NAME,R,ATYPS)\
  void* Function_new_ ## NAME ( R (*fp) ( UNPAREN(ATYPS) ) );

#define Function_call(NAME,R,ATYPS)\
  R Function_call_ ## NAME ( UNPAREN(ATYPS) );


Function_new(f1,void,())

Function_call(f1,void,(void* op))

Function_new(f2,void,(int x))

Function_call(f2,void,(void* op,int x))

Function_new(f3,int,(int x))

Function_call(f3,int,(void* op,int x))

Function_new(f4,int,(int x,char y))

Function_call(f4,int,(void* op,int x,char y))

Function_new(f5,int,())

Function_call(f5,int,(void* op))


#ifdef __cplusplus
}
#endif
