#define MACRODEF

#ifndef MACRO1
    #ifndef MACRO2
        #define VAL0 0
        #ifndef MACRO3
            #define VAL1 1
        #endif
        #define VAL2 2
        #ifndef MACRODEF
            #define VAL3 3
        #endif
        #define VAL4 4
    #endif
#endif

int main() {
    return VAL0 + VAL1 + VAL2 + VAL3 + VAL4;
}
