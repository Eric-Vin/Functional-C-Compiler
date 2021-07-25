#define SELF 9 + SELF
#define FOO 1 + BAR + 1
#define BAR 2 + FOO + 2

int main() {
    return SELF + FOO + BAR;
}
