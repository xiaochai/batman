package thinkinjava;

public class Hidden {
    private class MyFly implements Flyable {
        @Override
        public void fly() {}
    }
    public MyFly myFly(){return new MyFly();}

    public static void main(String[] args){
        Flyable flyable = new Hidden().myFly();
        new Flyable(){
            @Override
            public void fly() {
            }
        };
    }
}

interface Flyable {
    void fly();
}