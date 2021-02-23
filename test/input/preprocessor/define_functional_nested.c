#define MACRO1(x, y) (x + y)
#define MACRO2(a, b) (MACRO1(a ,b )    + MACRO1( b, a))

int main() {
    return MACRO2( 1,2);
}