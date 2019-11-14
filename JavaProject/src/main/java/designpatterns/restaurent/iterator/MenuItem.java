package designpatterns.restaurent.iterator;

public class MenuItem {
    public String name, desc;
    public double price;

    public MenuItem(String name, String desc, double price) {
        this.name = name;
        this.desc = desc;
        this.price = price;
    }
}
