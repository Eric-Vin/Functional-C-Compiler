#define NUMBER 1

#ifdef NUMBER
	#undef NUMBER
	#define NUMBER 2
#endif

int main() {
    return NUMBER;
}