extern void __assert_fail ( const char * __assertion , const char * __file , 
unsigned int __line , const char * __function ) 
__attribute__ ( ( __nothrow__ , __leaf__ ) ) __attribute__ ( ( __noreturn__ ) ) ; 
extern void __assert_perror_fail ( int __errnum , const char * __file , 
unsigned int __line , const char * __function ) 
__attribute__ ( ( __nothrow__ , __leaf__ ) ) __attribute__ ( ( __noreturn__ ) ) ; 
extern void __assert ( const char * __assertion , const char * __file , int __line ) 
__attribute__ ( ( __nothrow__ , __leaf__ ) ) __attribute__ ( ( __noreturn__ ) ) ; 
int main ( ) { 
return 7 ; 
} 
