package designpatterns.pizza.FactoryInterface;

import designpatterns.pizza.Pizza;

public interface PizzaFactory {
    public Pizza createPizza(String type);
}
