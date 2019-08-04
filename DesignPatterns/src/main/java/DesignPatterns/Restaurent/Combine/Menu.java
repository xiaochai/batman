package DesignPatterns.Restaurent.Combine;

import java.util.ArrayList;

public class Menu extends MenuComponent {
    private String name;
    private ArrayList<MenuComponent> menuComponents = new ArrayList<>();

    public Menu(String name) {
        this.name = name;
    }

    @Override
    public void add(MenuComponent menuComponent) {
        menuComponents.add(menuComponent);
    }

    @Override
    public void remove(MenuComponent menuComponent) {
        menuComponents.remove(menuComponent);
    }

    @Override
    public MenuComponent getChild(int i) {
        return menuComponents.get(i);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void print(){
        System.out.printf("===%s餐厅===\n", name);
        for(MenuComponent m: menuComponents){
            m.print();
        }
    }
}
