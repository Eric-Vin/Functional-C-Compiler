#define MACRODEF

#ifndef MACRO1
    #ifndef MACRO2
        #ifndef MACRO3
            #define VAL1 3
        #endif
        #ifndef MACRODEF
            #define VAL2 4
        #endif
    #endif
#endif

int main() {
    return VAL1 + VAL2;
}
