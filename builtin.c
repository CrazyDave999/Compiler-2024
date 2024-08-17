#include <stdio.h>
#include <stdbool.h>
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

void *CrazyDave_AllocArray(int size){
    int *p = malloc((size << 2) + 4);
    p[0] = size;
    return p + 1;
}

int CrazyDave_GetArraySize(void *arr){
    return ((int *)arr)[-1];
}

int string_length(char *str){
    return strlen(str);
}

char *string_substring(char *str, int left, int right){
    char *sub = malloc(right - left + 2);
    memcpy(sub, str + left, right - left);
    sub[right - left] = 0;
    return sub;
}

int string_parseInt(char *str){
    int n;
    sscanf(str, "%d", &n);
    return n;
}

int string_ord(char *str, int pos){
    return str[pos];
}

char *string_add(char *lhs, char *rhs){
    int len1 = strlen(lhs);
    int len2 = strlen(rhs);
    char *str = malloc(len1 + len2 + 1);
    memcpy(str, lhs, len1);
    memcpy(str + len1, rhs, len2 + 1);
    str[len1 + len2] = 0;
    return str;
}

bool string_eq(char *lhs, char *rhs){
    return strcmp(lhs, rhs) == 0;
}

bool string_ne(char *lhs, char *rhs){
    return strcmp(lhs, rhs) != 0;
}

bool string_lt(char *lhs, char *rhs){
    return strcmp(lhs, rhs) < 0;
}

bool string_le(char *lhs, char *rhs){
    return strcmp(lhs, rhs) <= 0;
}

bool string_gt(char *lhs, char *rhs){
    return strcmp(lhs, rhs) > 0;
}

bool string_ge(char *lhs, char *rhs){
    return strcmp(lhs, rhs) >= 0;
}