package designpatterns.state;

public class StateMachine {
    StateBegin stateBegin;
    StateIntPart stateIntPart;
    StateFloatPart stateFloatPart;
    State currentState;

    public StateMachine() {
        this.stateBegin = new StateBegin(this);
        this.stateFloatPart = new StateFloatPart(this);
        this.stateIntPart = new StateIntPart(this);
        this.currentState = this.stateBegin;
    }
    public void meetNumber(){
        currentState.meetNumber();
    }
    public void meetDot(){
        currentState.meetDot();
    }
    public void meetSpace(){
        currentState.meetSpace();
    }
}
