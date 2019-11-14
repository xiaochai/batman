package designpatterns.pizza;

public class NormalPizz extends Pizza {
    @Override
    public void prepare() {
        System.out.println("prepare normal");
    }
}
