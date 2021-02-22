#ifndef MACRO1
    #ifndef MACRO2
        #ifndef MACRO3
            #define MACRO4 6
    #endif
#endif

int main() {
    return MACRO4;
}
