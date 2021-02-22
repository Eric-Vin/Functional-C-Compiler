#define NUMBER 1

# ifdef NUMBER
    #define MACRO 4
# endif

int main() {
    return NUMBER + MACRO;
}
