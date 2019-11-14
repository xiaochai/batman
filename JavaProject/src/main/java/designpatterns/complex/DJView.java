package designpatterns.complex;


import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;

public class DJView implements ChangeListener, BeatObserver, BPMObserver {
    private DJModel djModel;
    private DJController djController;

    private JFrame jFrame;
    private JPanel jPanel;
    private JSlider jSlider;
    private JLabel jLabel;
    private JProgressBar jProgressBar;

    public DJView(DJController c, DJModel m) {
        djModel = m;
        djController = c;

        jFrame = new JFrame();
        jPanel = new JPanel(new GridLayout(3,1));

        jSlider=new JSlider(30,300);
        jSlider.addChangeListener(this);

        jLabel = new JLabel();
        jLabel.setText("Current BPM: 60");

        jProgressBar=new JProgressBar(0,100);
        jProgressBar.setValue(23);

        jPanel.add(jSlider);
        jPanel.add(jLabel);
        jPanel.add(jProgressBar);
        jFrame.add(jPanel);
        jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        jFrame.setSize(500,300);
        jFrame.setVisible(true);

        djModel.registerBeatObserver(this);
        djModel.registerBPMObserver(this);
    }

    @Override
    public void stateChanged(ChangeEvent e) {
        if(e.getSource() == jSlider){
            int bpm = jSlider.getValue();
            djController.setBPM(bpm);
        }
    }

    @Override
    public void onChange(int bpm) {
        jLabel.setText("Current BPM: " + bpm);
    }

    @Override
    public void onBeat() {
        for (int i = 0; i < 80; i++) {
            jProgressBar.setValue(i);
            try {
                java.lang.Thread.sleep(1);
            } catch (InterruptedException e) {
                System.out.println(e);
            }
        }
        for (int i = 80; i > 0; i--) {
            jProgressBar.setValue(i);
            try {
                java.lang.Thread.sleep(2);
            } catch (InterruptedException e) {
                System.out.println(e);
            }
        }
    }
}
