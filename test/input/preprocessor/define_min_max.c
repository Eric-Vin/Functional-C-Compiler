#define NUM1 12
#define NUM2 32

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

int main() {
    return (MIN(NUM1,NUM2) < MAX(NUM1, NUM2));
}
