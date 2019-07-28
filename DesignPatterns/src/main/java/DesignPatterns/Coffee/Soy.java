package DesignPatterns.Coffee;

public class Soy implements ICoffee {
    ICoffee coffee;

    public Soy(ICoffee coffee) {
        this.coffee = coffee;
    }

    @Override
    public double cost() {
        return 0.5 + coffee.cost();
    }

    @Override
    public String desc() {
        return coffee.desc() + " with soy";
    }
}
