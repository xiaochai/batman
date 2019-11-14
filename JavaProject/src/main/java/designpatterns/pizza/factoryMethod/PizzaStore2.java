package designpatterns.pizza.factoryMethod;

import designpatterns.pizza.NormalPizz;
import designpatterns.pizza.Pizza;
import designpatterns.pizza.VeggiePizza;

public class PizzaStore2 extends PizzaStoreBase {
    @Override
    public Pizza createPizza(String type) {
        Pizza pizza;
        if(type.equals("veggie")){
            pizza = new VeggiePizza();
        }else{
            pizza = new NormalPizz();
        }
        return pizza;
    }
}
