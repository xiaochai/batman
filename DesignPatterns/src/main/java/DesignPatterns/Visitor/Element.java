package DesignPatterns.Visitor;

public abstract class Element {
    protected String name;

    public String getName() {
        return name;
    }

    public abstract void accept(Visitor visitor);
}
