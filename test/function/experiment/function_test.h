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


#ifdef __cplusplus
}
#endif
