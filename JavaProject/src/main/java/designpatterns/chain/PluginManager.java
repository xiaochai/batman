package designpatterns.chain;

public class PluginManager {
    private Request request;
    private Handler headHandler;
    private Handler currHandler;

    public PluginManager(Request request) {
        this.request = request;
    }

    public void addPlugin(Handler handler) {
        if (headHandler == null) {
            headHandler = handler;
        } else {
            currHandler.setNextHandler(handler);
        }
        currHandler = handler;
    }

    public void run() {
        if (headHandler != null) {
            headHandler.handler(request);
        }
    }
}
