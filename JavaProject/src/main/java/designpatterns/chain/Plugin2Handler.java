package designpatterns.chain;

public class Plugin2Handler extends Handler {
    @Override
    public void handler(Request request) {
        System.out.println("Plugin2: before run all plugin");
        next(request);
        System.out.println("Plugin2: after run all plugin");
    }
}
