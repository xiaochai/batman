package DesignPatterns.WeatherData;

import java.util.ArrayList;

public class WeatherData implements Subject {
    private float temperature, humidity, pressure;
    private ArrayList<Observer> observers;

    public WeatherData(){
        observers = new ArrayList<Observer>();
    }

    public void setHumidity(float humidity) {
        this.humidity = humidity;
        this.notifyObserver();
    }
    public void setTemperature(float temp){
        this.temperature = temp;
        this.notifyObserver();
    }

    public void setPressure(float pressure) {
        this.pressure = pressure;
        this.notifyObserver();
    }

    public double getTemperature() {
        return temperature;
    }

    public double getHumidity() {
        return humidity;
    }

    public double getPressure() {
        return pressure;
    }

    @Override
    public void registerObserver(Observer o){
        observers.add(o);
    }
    @Override
    public void removeObserver(Observer o){
        int i = observers.indexOf(o);
        if(i >= 0){
            observers.remove(i);
        }
    }

    @Override
    public void notifyObserver() {
        for(int i = 0;i<observers.size();i++){
            Observer o = observers.get(i);
            o.update(temperature, humidity, pressure);
        }
    }
}
