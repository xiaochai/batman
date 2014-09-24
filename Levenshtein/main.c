#include<stdio.h>
#include<stdlib.h>
#include<string.h>

static int acount = 0, bcount = 0;
static int global_arr[100][100];
int levenshtein(char *a, char *b);
int levenshtein_r(char *a, char *b, int, int);
int main(int argc, char ** argv){
    if(argc != 3){
        printf("Args Error\n");
        return 1;
    }
    for(int i = 0; i < 100; i++){
        for(int j = 0; j < 100; j++)
            global_arr[i][j] = -1;
    }
    printf("%d\n", levenshtein(argv[1], argv[2]));
    printf("%d\n", levenshtein_r(argv[1], argv[2], strlen(argv[1]), strlen(argv[2])));
    printf("count:%d,%d\n", acount, bcount);
    return 0;
}

int levenshtein_r(char *a, char *b, int al, int bl){
    if(global_arr[al][bl] != -1)
        return global_arr[al][bl];
    acount ++;
    printf("%d, %d ----\n", al, bl);
    if(al == 0) return global_arr[al][bl] = bl;
    if(bl == 0) return global_arr[al][bl] = al;

    int add = levenshtein_r(a, b, al - 1, bl) + 1;
    int del = levenshtein_r(a, b, al, bl-1) + 1;
    int change = levenshtein_r(a, b, al - 1, bl - 1);
    if(*(a+al-1) != *(b+bl-1))
        change += 1;

    //取三个值中最小的一个
    //printf("!!!%d, %d, %d  !!!! %d, %d \n", add, del, change, al, bl);
    if(add < del){
        return global_arr[al][bl] = add < change ? add : change;
    }else{
        return global_arr[al][bl] = del < change ? del : change;
    }
}

void printMatrix(int *m, int len1, int len2, char *a, char *b){
    printf("\t%s\t", a);
    for(int i = 0; i < len1; i ++){
        printf("%c\t", a[i]);
    }
    printf("\n");
    for(int j = 0; j < len2+1; j++){
        if(j == 0){
            printf("%s\t", b);
        }else{
            printf("%c\t", b[j -1]);
        }
        for(int i = 0; i < len1+1; i++){
            printf("%d\t", *(m + j * (len1+1) + i));
        }
        printf("\n");
    }
    printf("\n\n");
}

int levenshtein(char *a, char *b){
    int len1 = strlen(a), len2 = strlen(b);
    int *matrix = (int*)malloc((len1+1) * (len2+1) *sizeof(int));
    for(int i = 0; i < len1 + 1; i++){//初使化第一行
        *(matrix+i) = i;
    }
    for(int j = 0; j < len2 + 1; j++){//初使化第一列
        *(matrix + j * (len1 +1)) = j;
    }

    int min;
    for(int i = 1; i < len1 + 1; i++){
        for(int j = 1; j < len2 + 1; j++){
            bcount ++;
            //int up = matrix[j-1][i] + 1;
            int up = *(matrix + (j-1) * (len1 +1) + i) + 1;
            //int left = matrix[j][i-1] + 1;
            int left = *(matrix + j * (len1+1) + i-1) + 1;
            //int corner = matrix[j-1][i-1] + (a[i-1] == b[j-1] ? 0 : 1);
            int corner = *(matrix + (j-1)*(len1+1) + i-1) + (a[i-1] == b[j-1] ? 0 : 1);
            min = up;
            if(left < min) min = left;
            if(corner < min) min = corner;
            //matrix[j][i] = min;
            *(matrix + j * (len1 + 1) + i) = min;
        }
    }
    printMatrix(matrix, len1, len2, a, b );
    free(matrix);
    return min;
}
