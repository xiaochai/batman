extern int compare, swap;
void insert_sort(int * a, int n){ 
    swap = 0;
    compare = 0;
    for(int i = 1; i < n; i++){
        if(a[i] < a[i - 1]){
            int t = a[i];
            int j = i - 1;
            a[i] = a[j];
            compare ++;
            while(j > 0 && a[j - 1] > t){
                compare ++;
                swap ++;
                a[j] = a[--j];
            }
            a[j] = t;
            swap ++;
        }
    }
}

void insert_sort_bin(int * a, int n){
    swap = 0;
    compare = 0;
    for(int i = 1; i < n; i++){
        if(a[i] < a[i - 1]){
            int left = 0, right = i - 1, mid;
            int t = a[i];
            compare ++;
            while(left <= right){
                mid = (left + right) / 2;
                compare ++;
                if(t >= a[mid]){
                    left = mid + 1;
                }else{
                    right = mid - 1;
                }
            }
            left = i;
            while(left > right + 1){
                a[left] = a[--left];
                swap ++;
            }
            a[right + 1] = t;
        }
    }
}
