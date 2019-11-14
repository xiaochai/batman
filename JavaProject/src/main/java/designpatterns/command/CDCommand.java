package designpatterns.command;

public class CDCommand implements Command{
    private CD cd;

    public CDCommand(CD cd) {
        this.cd = cd;
    }

    @Override
    public void execute() {
        cd.setCD();
        cd.setVolume(11);
        cd.play();
    }
}
