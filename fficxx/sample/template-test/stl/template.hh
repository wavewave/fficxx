#include <iostream>
#include <vector>

template <class T>
void printout ( std::vector<T>* xs ) {
  for( auto x : *xs ) { 
    std::cout << x << std::endl;
  }
}

#define w_printout(T)                                                   \
    extern "C" {                                                        \
	void w_printout_ ## T ( void* xs );                             \
    }                                                                   \
    inline void w_printout_ ## T ( void* xs ) {		                \
	std::vector<T>* xs0 = reinterpret_cast<std::vector<T>* >(xs);	\
	printout( xs0 );                                                \
    }                                                                   \
    auto a_printout_ ## T = w_printout_ ## T  ; 

#define w_push_back(T)                                                  \
    extern "C" {                                                        \
	void w_push_back_ ## T ( void* xs, T x );	           	\
    }                                                                   \
    inline void w_push_back_ ## T ( void* xs, T x ) {			\
	std::vector<T>* xs0 = reinterpret_cast<std::vector<T>* >(xs);	\
	xs0->push_back( x );             				\
    }                                                                   \
    auto a_push_back_ ## T = w_push_back_ ## T  ; 


#define w_new(T)                                                     \
    extern "C" {                                                        \
	void* w_new_ ## T ( void );	                        	\
    }                                                                   \
    inline void* w_new_ ## T () {	                 		\
        std::vector<T>* xs = new std::vector<T>();	          	\
	return reinterpret_cast<void*>(xs);                             \
    }                                                                   \
    auto a_new_ ## T = w_new_ ## T  ; 


#define w_at(T)                                                         \
    extern "C" {                                                        \
	T* w_at_ ## T ( void*, int );	                        	\
    }                                                                   \
    inline T* w_at_ ## T ( void* v, int i ) {	              		\
	std::vector<T>* v1 = reinterpret_cast<std::vector<T>* >(v);     \
	return &(v1->at(i));		                         	\
    }                                                                   \
    auto a_at_ ## T = w_at_ ## T  ; 


#define w_delete(T)                                                     \
    extern "C" {                                                        \
	void w_delete_ ## T ( void* );	                        	\
    }                                                                   \
    inline void w_delete_ ## T ( void* v ) {	                 	\
        std::vector<T>* xs = reinterpret_cast<std::vector<T>* >(v);     \
	delete xs;                              			\
    }                                                                   \
    auto a_delete_ ## T = w_delete_ ## T  ; 

