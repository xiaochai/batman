package DesignPatterns.Flyweight;

public class StarManager {
    private int count;
    private int[] brightness, xs, ys;
    private Star star;

    public StarManager(int count) {
        this.count = count;
        star = new Star();
        brightness = new int[count];
        xs = new int[count];
        ys = new int[count];
        for (int i = 0; i < count; i++) {
            brightness[i] = (int) (Math.random());
            xs[i] = (int) (Math.random());
            ys[i] = (int) (Math.random());
        }
    }

    public void draw() {
        for (int i = 0; i < count; i++) {
            star.display(xs[i], ys[i], brightness[i]);
        }
    }
}