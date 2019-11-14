package designpatterns.pizza.factoryInterface;

import designpatterns.pizza.Pizza;

public interface PizzaFactory {
    public Pizza createPizza(String type);
}
