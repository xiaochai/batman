package DesignPatterns.State;

public class StateFloatPart extends State {
    public StateFloatPart(StateMachine stateMachine) {
        super(stateMachine);
    }

    @Override
    public void meetNumber() {
    }

    @Override
    public void meetSpace() {
        stateMachine.currentState = stateMachine.stateBegin;
    }
}
