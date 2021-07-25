#define MACRO1 "foobar"
#define MACRO2(x, y) x + y

int main() {
    return MACRO1 + MACRO1(x,y) + MACRO2 + MACRO2(x,y);
}

#define MACRO2 "foobar"
#define MACRO1(x, y) x + y

void fun() {
    return MACRO1 + MACRO1(x,y) + MACRO2 + MACRO2(x,y);
}
