package designpatterns.coffee;

public class Coffee implements ICoffee {

    @Override
    public double cost() {
        return 4.0;
    }

    @Override
    public String desc() {
        return "coffee";
    }
}
