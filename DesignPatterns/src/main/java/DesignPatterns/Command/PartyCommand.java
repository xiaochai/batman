package DesignPatterns.Command;

import java.util.HashMap;
import java.util.Map;

public class PartyCommand implements Command {
    private CD cd;
    private Light light;
    public PartyCommand(CD cd, Light light) {
    }
    @Override
    public void execute() {
        cd.setCD();
        cd.setVolume(20);
        cd.play();
        light.on();
    }

}
