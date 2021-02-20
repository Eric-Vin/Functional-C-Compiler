#define MACRO1
#define MACRO2

#ifdef MACRO1
    #ifdef MACRO2
        #define MACRO3 6
    #endif
#endif

int main() {
    return MACRO3;
}
