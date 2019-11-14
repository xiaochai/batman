package designpatterns.bridge;

public abstract class OS {
    Software software;

    public OS(Software software) {
        this.software = software;
    }

    public abstract void run();
}
