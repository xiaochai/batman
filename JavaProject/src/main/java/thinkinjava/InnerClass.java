package thinkinjava;

public class InnerClass {
    public class Sub {
        public InnerClass f() {
            return InnerClass.this;
        }
    }

    public static class StaticSub {}

    public static void main(String[] args) {
        InnerClass innerClass = new InnerClass();
        InnerClass.Sub sub = innerClass.new Sub();
        System.out.println(sub.f() == innerClass); // true

        StaticSub staticSub = new InnerClass.StaticSub();
    }
}
