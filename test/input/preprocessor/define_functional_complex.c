#define MACRO(x, y,z ) x - y * z + y + z - x

int main() {
    return MACRO( 1,2, 3);
}