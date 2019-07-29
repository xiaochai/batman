package DesignPatterns.Pizza.FactoryInterface;

import DesignPatterns.Pizza.Pizza;

public interface PizzaFactory {
    public Pizza createPizza(String type);
}
