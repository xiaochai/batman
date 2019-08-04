package DesignPatterns.Restaurent;

import java.util.ArrayList;

public class MenuOfA {
    ArrayList<MenuItem> menuItems;

    public MenuOfA() {
        menuItems = new ArrayList<>();
        addItem("拉面", "兰州拉面", 9.9);
        addItem("冷面", "延吉冷面", 19.9);
    }

    public void addItem(String name, String desc, double price) {
        MenuItem i = new MenuItem(name, desc, price);
        menuItems.add(i);
    }

    public ArrayList<MenuItem> getMenuItems(){
        return menuItems;
    }
}
