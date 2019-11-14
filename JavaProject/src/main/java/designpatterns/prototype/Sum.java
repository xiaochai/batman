package designpatterns.prototype;

public class Sum implements Cloneable {
    private int total;

    public Sum(int max) {
        for(int i=0;i<max;i++){
            total += i;
        }
    }

    public int getTotal() {
        return total;
    }
    @Override
    public Sum clone() throws CloneNotSupportedException{
        return (Sum)super.clone();
    }
}
