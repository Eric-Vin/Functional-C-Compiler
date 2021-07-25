#define MACRO(x,y) x+y

int main() {
    return MACRO(,) + MACRO( ,) + MACRO(, ) + MACRO( , );
}
