package DesignPatterns.State;

public class State {
    StateMachine stateMachine;
    public State(StateMachine stateMachine) {
        this.stateMachine = stateMachine;
    }

    public void meetNumber(){
        throw new UnsupportedOperationException();
    }

    public void meetDot(){
        throw new UnsupportedOperationException();
    }

    public void meetSpace(){
        throw new UnsupportedOperationException();
    }
}
