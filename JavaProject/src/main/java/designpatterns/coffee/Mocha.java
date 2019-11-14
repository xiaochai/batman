package designpatterns.coffee;

public class Mocha implements ICoffee {
    ICoffee coffee;

    public Mocha(ICoffee coffee) {
        this.coffee = coffee;
    }

    @Override
    public double cost() {
        return 0.4 + coffee.cost();
    }

    @Override
    public String desc() {
        return coffee.desc() + " with mocha";
    }
}
