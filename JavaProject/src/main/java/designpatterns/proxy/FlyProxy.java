package designpatterns.proxy;

public class FlyProxy extends Fly {
    Fly fly;

    public FlyProxy(Fly fly) {
        this.fly = fly;
    }

    @Override
    public void dofly(){
        System.out.println("do prepare");
        fly.dofly();
        System.out.println("finish");
    }
}
