package designpatterns.command;

public class RemoteController {
    Command[] commands;

    public RemoteController() {
        commands = new Command[3];
        Command noCommand = new NoCommand();
        for (int i = 0; i < 3; i++) {
            commands[i] = noCommand;
        }
    }

    public void setCommands(int slot, Command command) {
        commands[slot] = command;
    }

    public void buttonPush(int slot) {
        commands[slot].execute();
    }
}
