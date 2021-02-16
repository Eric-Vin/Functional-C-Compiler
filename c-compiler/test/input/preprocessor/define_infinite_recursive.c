#define SELF SELF
#define FOO BAR + 1
#define BAR FOO + 2

int main() {
    return SELF + FOO + BAR;
}
