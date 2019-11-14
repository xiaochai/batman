package designpatterns.pizza;

public class PizzaStore {
    SimplePizzaFactory simplePizzaFactory;

    public PizzaStore(SimplePizzaFactory simplePizzaFactory) {
        this.simplePizzaFactory = simplePizzaFactory;
    }

    public Pizza orderPizza(String type){
        Pizza pizza = simplePizzaFactory.createPizza(type);
        pizza.prepare();
        pizza.bake();
        pizza.cut();
        pizza.box();
        return pizza;
    }

    public Pizza orderPizzaOld(String type){
        Pizza pizza;
        if(type.equals("cheese")){
            pizza = new CheesePizza();
        }else if(type.equals("veggie")){
            pizza = new VeggiePizza();
        }else{
            pizza = new NormalPizz();
        }
        pizza.prepare();
        pizza.bake();
        pizza.cut();
        pizza.box();
        return pizza;
    }
}
