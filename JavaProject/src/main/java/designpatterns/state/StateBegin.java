package designpatterns.state;

public class StateBegin extends State {
    public StateBegin(StateMachine stateMachine) {
        super(stateMachine);
    }

    @Override
    public void meetNumber() {
        this.stateMachine.currentState = this.stateMachine.stateIntPart;
    }

    @Override
    public void meetDot() {
        this.stateMachine.currentState = stateMachine.stateFloatPart;
    }
}
