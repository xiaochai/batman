package DesignPatterns.Chain;

public class Plugin1Handler extends Handler {
    @Override
    public void handler(Request request) {
        System.out.println("Plugin1: before run all plugin");
        next(request);
        System.out.println("Plugin1: after run all plugin");

    }
}
