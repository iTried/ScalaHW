import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;


public class Captcha {


    static class Generator implements Runnable {

        private String queue;

        public Generator(String queue) {
            this.queue = queue;
            queue = queue + "fdfs";
            System.out.println(queue);
        }

        public void run() {
            synchronized(queue) {
                if (queue.length() <= 40) {
                    System.out.println("gen : adding elements to queue");
                }else{
                    try{
                        Thread.sleep(500);
                    }catch(InterruptedException ignore){
                        /*NOP*/
                    }
                }
            }
        }
    }

    public static void main(String[] args) {
        String queue = "";
        Thread gen1 = new Thread(new Generator(queue));
        //Thread gen2 = new Thread(new Generator(queue));
        gen1.start();


        try{
            System.out.println("main::Sleeping");
            Thread.sleep(500);
        }catch(InterruptedException ignore){
            /*NOP*/
        }
        synchronized (queue){
            queue.notify();
            System.out.println(queue);
        }
    }
}
