package designpatterns.pizza.FactoryInterface;

import designpatterns.pizza.CheesePizza;
import designpatterns.pizza.NormalPizz;
import designpatterns.pizza.Pizza;

public class PizzaFactory1 implements PizzaFactory {
    @Override
    public Pizza createPizza(String type) {
        Pizza pizza;
        if (type.equals("cheese")) {
            pizza = new CheesePizza();
        } else {
            pizza = new NormalPizz();
        }
        return pizza;
    }
}
