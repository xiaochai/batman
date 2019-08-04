package DesignPatterns.Restaurent.Combine;

public class MenuItem extends MenuComponent {
    private String name, desc;
    private Double price;

    public MenuItem(String name, String desc, Double price) {
        this.name = name;
        this.desc = desc;
        this.price = price;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getDesc() {
        return desc;
    }

    @Override
    public Double getPrice() {
        return price;
    }

    @Override
    public void print() {
        System.out.printf("%s, %s, %f\n", name, desc, price);
    }
}
