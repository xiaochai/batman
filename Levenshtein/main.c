#include<stdio.h>
#include<stdlib.h>
#include<string.h>

/**
 * 递归的求解问题
 */
static int acount = 0;
int Levenshtein_r(char *s1, char *s2, int len1, int len2){
    acount ++;//做为统计的参数
    if(len1 == 0) return len2;
    if(len2 == 0) return len1;

    int add = Levenshtein_r(s1, s2, len1 - 1, len2) + 1;
    int del = Levenshtein_r(s1, s2, len1, len2 - 1) + 1;
    int sub = Levenshtein_r(s1, s2, len1 - 1, len2 - 1) +
        (*(s1+len1-1) == *(s2+len2-1) ? 0 : 1);

    //取三个值中最小的一个
    if(add < del){
        return add < sub ? add : sub;
    }else{
        return del < sub ? del : sub;
    }
}


static int bcount = 0;
static int global_arr[100][100];
/**
 * 递归带缓存的求解问题,分为两个函数，第二个函数初使化全局数组
 * 第一个函数递归求值
 */
int _Levenshtein_r_c(char *s1, char *s2, int len1, int len2){
    //读取缓存
    if(global_arr[len1][len2] != -1)
        return global_arr[len1][len2];
    bcount ++;//做为统计的参数

    if(len1 == 0) return global_arr[len1][len2] = len2;
    if(len2 == 0) return global_arr[len1][len2] = len1;

    int add = _Levenshtein_r_c(s1, s2, len1 - 1, len2) + 1;
    int del = _Levenshtein_r_c(s1, s2, len1, len2 - 1) + 1;
    int sub = _Levenshtein_r_c(s1, s2, len1 - 1, len2 - 1) +
        (*(s1+len1-1) == *(s2+len2-1) ? 0 : 1);

    //取三个值中最小的一个
    if(add < del){
        return global_arr[len1][len2] = add < sub ? add : sub;
    }else{
        return global_arr[len1][len2] = del < sub ? del : sub;
    }
}
int Levenshtein_r_c(char *s1, char *s2, int len1, int len2){
    for(int i = 0; i < 100; i++){
        for(int j = 0; j < 100; j++)
            global_arr[i][j] = -1;
    }
    return _Levenshtein_r_c(s1, s2, len1, len2);
}


static int ccount = 0;
/**
 * 使用表格的方式求值
 */
int Levenshtein(char *s1, char *s2, int len1, int len2){
    int *matrix = (int*)malloc((len1+1) * (len2+1) *sizeof(int));
    for(int i = 0; i < len1 + 1; i++){//初使化第一行
        *(matrix+i) = i;
        ccount ++;//统计使用
    }
    for(int j = 0; j < len2 + 1; j++){//初使化第一列
        *(matrix + j * (len1 +1)) = j;
        ccount ++;//统计使用
    }

    int min;
    for(int i = 1; i < len1 + 1; i++){
        for(int j = 1; j < len2 + 1; j++){
            ccount ++;//统计使用
            //插入操作的距离
            int add = *(matrix + (j-1) * (len1 +1) + i) + 1;
            //删除操作的距离
            int del = *(matrix + j * (len1+1) + i-1) + 1;
            //替换操作的距离
            int sub = *(matrix + (j-1)*(len1+1) + i-1) + (s1[i-1] == s2[j-1] ? 0 : 1);
            //取三都最小的值
            min = add;
            if(del < min) min = del;
            if(sub < min) min = sub;

            *(matrix + j * (len1 + 1) + i) = min;
        }
    }
    free(matrix);
    return min;
}


static int dcount = 0;
/**
 * 使用向量的方式求值
 */
int Levenshtein_v(char *s1, char *s2, int len1, int len2){
    //初使化两个向量
    int *v1 = (int*)malloc((len2+1) * sizeof(int));
    int *v2 = (int*)malloc((len2+1) * sizeof(int));
    for(int j = 0; j < len2 + 1; j++){//初使化第一列
        *(v1 + j) = j;
        dcount ++;//统计使用
    }

    int min;
    for(int i = 1; i < len1 + 1; i++){
        *v2 = i;
        dcount ++;//统计使用
        for(int j = 1; j < len2 + 1; j++){
            dcount ++;//统计使用
            //插入操作的距离
            int add = *(v2 + j - 1) + 1;
            //删除操作的距离
            int del = *(v1 + j) + 1;
            //替换操作的距离
            int sub = *(v1 + j - 1) + (s1[i-1] == s2[j-1] ? 0 : 1);
            //取三都最小的值
            min = add;
            if(del < min) min = del;
            if(sub < min) min = sub;

            *(v2 + j) = min;
        }
        int *t = v1;
        v1 = v2;
        v2 = t;
    }
    free(v1);
    free(v2);
    return min;
}

int main(int argc, char ** argv){
    if(argc != 3){
        printf("Args Error\n");
        return 1;
    }
    printf("%d\n", Levenshtein(argv[1], argv[2], strlen(argv[1]), strlen(argv[2])));
    printf("%d\n", Levenshtein_r(argv[1], argv[2], strlen(argv[1]), strlen(argv[2])));
    printf("%d\n", Levenshtein_r_c(argv[1], argv[2], strlen(argv[1]), strlen(argv[2])));
    printf("%d\n", Levenshtein_v(argv[1], argv[2], strlen(argv[1]), strlen(argv[2])));
    printf("count:%d,%d, %d, %d\n", acount, bcount, ccount, dcount);
    return 0;
}

