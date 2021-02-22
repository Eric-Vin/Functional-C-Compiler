#ifndef MACRO1
    #ifndef MACRO2
        #define MACRO3 6
    #endif
#endif

int main() {
    return MACRO3;
}
