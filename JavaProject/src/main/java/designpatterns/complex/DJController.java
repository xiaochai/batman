package designpatterns.complex;

public class DJController {
    DJModel djModel;
    DJView djView;

    public DJController(DJModel djModel) {
        this.djModel = djModel;
        djView = new DJView(this, djModel);
    }

    public void setBPM(int bpm){
        djModel.setBPM(bpm);
    }

    public static  void main(String[] args){
        DJModel djModel = new DJModel();
        new DJController(djModel);
        djModel.start();
    }
}
