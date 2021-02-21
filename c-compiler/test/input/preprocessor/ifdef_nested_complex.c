#define MACRO1 1
#define MACRO2
#define MACRO3 1

#ifdef MACRO1
    #ifdef MACRO2
        #define VAL0 0
        #ifdef MACRO3
            #define VAL1 1
        #endif
        #define VAL2 2
        #ifdef MACROUNDEFINED
            #define VAL3 3
        #endif
        #define VAL4 4
    #endif
#endif

int main() {
    return VAL0 + VAL1 + VAL2 + VAL3 + VAL4;
}
