package designpatterns.coffee;

public class Milk implements ICoffee {
    ICoffee coffee;

    public Milk(ICoffee coffee) {
        this.coffee = coffee;
    }

    @Override
    public double cost() {
        return 0.3 + coffee.cost();
    }

    @Override
    public String desc() {
        return coffee.desc() + " with milk";
    }
}
