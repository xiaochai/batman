package designpatterns.bridge;

public class OSA extends OS {
    public OSA(Software software) {
        super(software);
    }

    @Override
    public void run() {
        System.out.println("osa");
        software.run();
    }
}
