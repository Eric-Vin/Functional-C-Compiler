#define NUMBER 3

#ifdef NUMBER
	#undef NUMBER
	#define NUMBER 4
#endif

int main() {
    return NUMBER;
}
