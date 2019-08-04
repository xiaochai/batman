package DesignPatterns.Bridge;

public class OSB extends OS {
    public OSB(Software software) {
        super(software);
    }

    @Override
    public void run() {
        System.out.println("osb");
        software.run();
    }
}
