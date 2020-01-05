package mythread;

import java.io.IOException;
import java.io.PipedReader;
import java.io.PipedWriter;

public class Pipe {
    public static void main(String[] args) throws IOException {
        PipedReader pipedReader = new PipedReader();
        PipedWriter pipedWriter = new PipedWriter();
        pipedWriter.connect(pipedReader);
        pipedWriter.write("ABCDEFG");
        while(true) {
            System.out.print((char)pipedReader.read());
        }
        // ABCDEFG
    }
}
