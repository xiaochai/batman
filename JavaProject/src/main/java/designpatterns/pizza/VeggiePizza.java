package designpatterns.pizza;

public class VeggiePizza extends Pizza {
    @Override
    public void prepare() {
        System.out.println("prepare veggie");
    }
}
