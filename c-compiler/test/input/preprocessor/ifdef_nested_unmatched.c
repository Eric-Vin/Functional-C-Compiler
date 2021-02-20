#define MACRO1 1
#define MACRO2 1
#define MACRO3 1

#ifdef MACRO1
    #ifdef MACRO2
            #ifdef MACRO3
                #define MACRO4 6
    #endif
#endif

int main() {
    return MACRO4;
}
