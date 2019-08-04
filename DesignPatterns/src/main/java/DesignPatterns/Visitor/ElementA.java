package DesignPatterns.Visitor;

public class ElementA extends Element {
    public ElementA() {
        name = "A";
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}
