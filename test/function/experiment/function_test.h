#ifdef __cplusplus
extern "C" {
#endif

#define VARGS(...) __VA_ARGS__

#define TAIL(x,...) __VA_ARGS__

#define UNPAREN(x) VARGS x


#define Function_new_decl(NAME,R,ATYPS)\
  void* Function_new_ ## NAME ( R (*fp) ( UNPAREN(ATYPS) ) );

#define Function_call_decl(NAME,R,ATYPS)\
  R Function_call_ ## NAME ( UNPAREN(ATYPS) );


#define Function_new_inst(NAME,R,ATYPS)                     \
    inline void* Function_new_ ## NAME ( R (*fp)( UNPAREN(ATYPS) ) ) \
  {\
      std::function< R ( UNPAREN(ATYPS) )>* p = new std::function< R ( UNPAREN(ATYPS) ) >(fp); \
    return static_cast<void*>(p);\
  }\
  auto a_Function_new_ ## NAME = Function_new_ ## NAME;


#define Function_call_inst(NAME,R,ATYPS,AVARS)        \
  inline R Function_call_ ## NAME ( UNPAREN(ATYPS) ) \
  {\
      std::function< R (TAIL ATYPS) >* p = static_cast< std::function< R (TAIL ATYPS) >* >(op); \
    return (*p) AVARS;                                 \
  }\
  auto a_Function_call_ ## NAME = Function_call_ ## NAME;


Function_new_decl(f1,void,())

Function_call_decl(f1,void,(void* op))

Function_new_decl(f2,void,(int x))

Function_call_decl(f2,void,(void* op,int x))

Function_new_decl(f3,int,(int x))

Function_call_decl(f3,int,(void* op,int x))

Function_new_decl(f4,int,(int x,char y))

Function_call_decl(f4,int,(void* op,int x,char y))

Function_new_decl(f5,int,())

Function_call_decl(f5,int,(void* op))


#ifdef __cplusplus
}
#endif
