#define MACRO1 1
#define MACRO2 1
#define MACRO3 1

#ifdef MACRO1
    #ifdef MACRO2
        #ifdef MACRO3
            #define VAL1 3
        #endif
        #ifdef MACROUNDEFINED
            #define VAL2 4
        #endif
    #endif
#endif

int main() {
    return VAL1 + VAL2;
}
