extern int compare, swap;
void bubble_sort(int *a, int n){
    compare = 0;
    swap = 0;
    for(int i = 0; i < n - 1; i++){
        for(int j = 0; j < n - 1 - i; j++){
            compare ++;
            if(a[j] > a[j + 1]){
                swap ++;
                int t = a[j + 1];
                a[j + 1] = a[j];
                a[j] = t;
            }
        }
    }
}


void bubble_sort_2(int *a, int n){
    compare = 0;
    swap = 0;
    for(int i = 0; i < n - 1; i++){
        for(int j = i + 1; j < n; j++){
            compare ++;
            if(a[i] > a[j]){
                swap ++;
                int t = a[i];
                a[i] = a[j];
                a[j] = t;
            }
        }
    }
}
