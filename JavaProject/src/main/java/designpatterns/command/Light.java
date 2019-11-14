package designpatterns.command;

public class Light {
    protected String desc;
    public void on(){
        System.out.printf("turn on %s light\n", desc);
    }
    public void off(){
        System.out.printf("turn off %s light\n", desc);
    }
}
