#include<stdio.h>
#include<time.h>
#include<sys/time.h>
#include<stdlib.h>
#include"main.h"

#define N 20//数组的大小

int compare = 0, swap = 0;//比较次数和交换次数

//打印出数组里的元素，用于查看是否排序正确
void printArray(int * a, int n){ 
    for(int i = 0; i < n; i++)
        printf("%d\n", a[i]);
    printf("\n\n");
}

//打印出排序一信息，如交换次数，使用时间等信息
void sort(int *a, int n, void (s)(int*, int), int print){
    //重新复制一个数据，防止破坏原数组
    int *b = (int *) malloc(n * sizeof(int));
    for(int i = 0; i < n; i++){
        b[i] = a[i];
    }   

    struct timeval start_t, end_t;
    gettimeofday(&start_t, NULL);
    s(b, n); 
    gettimeofday(&end_t, NULL);
    float t = end_t.tv_sec - start_t.tv_sec + (end_t.tv_usec - start_t.tv_usec) / 1000000.0;// 获取执行时间

    //输出执行信息
    printf("time:%f, compare:%d, swap:%d---------\n", t, compare, swap);
    if(print)
        printArray(b, n); 
    free(b);
}

int main(){
    int a[N];
    srand((unsigned)time(NULL));
    for(int i = 0; i < N; i++){
        a[i] = rand();
    }
    sort(a, N, bubble_sort, 1);
    sort(a, N, bubble_sort_2, 1);
    sort(a, N, insert_sort, 0);
    sort(a, N, insert_sort_bin, 0);
}
