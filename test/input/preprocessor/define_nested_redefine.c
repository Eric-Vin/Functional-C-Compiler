#define MACROONE 671
#define MACROTWO MACROONE
#undef MACROONE
#define MACROONE 231

int main() { 
    return MACROONE ;
}
