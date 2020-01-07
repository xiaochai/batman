package thinkinjava;

public class Base {
    public Base() {
        f();
    }

    public void f() {}

    public static void main(String[] args) {
        new Heris(20);
        // Heris i:0
        // Constructor i:20
    }
}

class Heris extends Base {
    public int i = 10;

    public Heris(int i) {
        super();
        this.i = i;
        System.out.println("Constructor i:" + this.i);
    }

    @Override
    public void f() {
        System.out.println("Heris i:" + i);
    }
}

