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


#define Function_new(NAME,R,...)\
  void* Function_new_ ## NAME ( R (*fp) ( __VA_ARGS__ ) );

#define Function_call(NAME,R,...)\
  R Function_call_ ## NAME ( void* fp, ##__VA_ARGS__ );


Function_new(f1,void)

Function_call(f1,void)


//Function_new(f2,void,int)

//Function_call(f2,void,int)


//Function_new(f5,int)

//Function_call(f5,int)


#ifdef __cplusplus
}
#endif
