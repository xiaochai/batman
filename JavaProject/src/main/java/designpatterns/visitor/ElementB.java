package designpatterns.visitor;

public class ElementB extends Element {
    public ElementB() {
        name = "B";
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}
