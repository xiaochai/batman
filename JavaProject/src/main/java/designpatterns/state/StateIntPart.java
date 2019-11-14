package designpatterns.state;

public class StateIntPart extends State {
    public StateIntPart(StateMachine stateMachine) {
        super(stateMachine);
    }

    @Override
    public void meetNumber() {
    }

    @Override
    public void meetDot() {
        stateMachine.currentState = stateMachine.stateFloatPart;
    }

    @Override
    public void meetSpace() {
        stateMachine.currentState = stateMachine.stateFloatPart;
    }
}
