package DesignPatterns.Pizza.FactoryInterface;

import DesignPatterns.Pizza.Pizza;

public abstract class PizzaStoreBase {

    PizzaFactory pizzaFactory;

    public PizzaStoreBase(PizzaFactory pizzaFactory) {
        this.pizzaFactory = pizzaFactory;
    }

    public Pizza orderPizza(String type) {
        Pizza pizza = pizzaFactory.createPizza(type);
        pizza.prepare();
        pizza.bake();
        pizza.cut();
        pizza.box();
        return pizza;
    }
}
