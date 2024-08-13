#include <stdio.h>
int printf(const char *pattern, ...);
int sprintf(char *dest, const char *pattern, ...);
int scanf(const char *pattern, ...);
int sscanf(const char *src, const char *pattern, ...);
size_t strlen(const char *str);
int strcmp(const char *s1, const char *s2);
void *memcpy(void *dest, const void *src, size_t n);
void *malloc(size_t n);

void print(char *str){
    printf("%s", str);
}

void println(char *str){
    printf("%s\n", str);
}

void printInt(int n){
    printf("%d", n);
}

void printlnInt(int n){
    printf("%d\n", n);
}

char *getString(){
    char *str = malloc(4096);
    scanf("%s", str);
    return str;
}

int getInt(){
    int n;
    scanf("%d", &n);
    return n;
}

char *toString(int n){
    char *str = malloc(64);
    sprintf(str, "%d", n);
    return str;
}

void *allocPtr(int size){
    int *p = malloc((size << 2) + 4);
    p[0] = size;
    return p + 1;
}