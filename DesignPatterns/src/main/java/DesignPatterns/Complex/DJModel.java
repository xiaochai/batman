package DesignPatterns.Complex;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class DJModel {
    private Integer bpm = 60;

    private List<BPMObserver> bpmObservers;
    private List<BeatObserver> beatObservers;

    public DJModel() {
        bpmObservers = new ArrayList<>();
        beatObservers = new ArrayList<>();
    }

    public void setBPM(int bpm) {
        synchronized (this.bpm) {
            this.bpm = bpm;
        }
        bpmNotify();
    }

    public void incrBPM(int i){
        synchronized (this.bpm) {
            this.bpm = this.bpm+i;
        }
        bpmNotify();
    }

    public Integer getBpm() {
        return bpm;
    }

    protected void beatNotify() {
        for (BeatObserver beatObserver : beatObservers) {
            beatObserver.onBeat();
        }
    }
    protected void bpmNotify(){
        for (BPMObserver bpmObserver : bpmObservers) {
            bpmObserver.onChange(bpm);
        }
    }

    public void start() {
        ExecutorService executorService = Executors.newSingleThreadExecutor();
        executorService.execute(() -> {
            int bpm;
            do {
                synchronized (this.bpm) {
                    bpm = this.bpm;
                }
                this.beatNotify();
                try {
                    java.lang.Thread.sleep(60000 / bpm);
                } catch (InterruptedException e) {
                    System.out.println(e);
                }
            } while (true);
        });
    }

    public void registerBPMObserver(BPMObserver bpmObserver) {
        if (!bpmObservers.contains(bpmObserver)) {
            bpmObservers.add(bpmObserver);
        }
    }

    public void registerBeatObserver(BeatObserver beatObserver) {
        if (!beatObservers.contains(beatObserver)) {
            beatObservers.add(beatObserver);
        }
    }

    public void removeBPMObserver(BPMObserver bpmObserver) {
        bpmObservers.remove(bpmObserver);
    }

    public void removeBeatObserver(BeatObserver beatObserver) {
        beatObservers.remove(beatObserver);
    }
}
