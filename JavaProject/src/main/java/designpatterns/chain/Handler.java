package designpatterns.chain;

public abstract class Handler {
    protected Handler nextHandler;

    public void setNextHandler(Handler nextHandler) {
        this.nextHandler = nextHandler;
    }

    public void next(Request request) {
        if (this.nextHandler != null) {
            this.nextHandler.handler(request);
        }
    }

    public abstract void handler(Request request);
}
