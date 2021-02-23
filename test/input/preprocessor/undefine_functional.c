#define MACRO(x, y) x + y
#undef MACRO

int main() {
    return MACRO(1,2);
}