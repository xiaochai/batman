package designpatterns.pizza.FactoryMethod;

import designpatterns.pizza.Pizza;

public abstract class PizzaStoreBase {
    public Pizza orderPizza(String type){
        Pizza pizza = createPizza(type);
        pizza.prepare();
        pizza.bake();
        pizza.cut();
        pizza.box();
        return pizza;
    }

    public abstract Pizza createPizza(String type);
}
