package designpatterns.pizza.factoryInterface;

import designpatterns.pizza.NormalPizz;
import designpatterns.pizza.Pizza;
import designpatterns.pizza.VeggiePizza;

public class PizzaFactory2 implements PizzaFactory {
    @Override
    public Pizza createPizza(String type) {
        Pizza pizza;
        if (type.equals("veggie")) {
            pizza = new VeggiePizza();
        } else {
            pizza = new NormalPizz();
        }
        return pizza;
    }
}
