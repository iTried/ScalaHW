import java.io.Console;
import java.util.ArrayList;
import java.util.List;

public class Captcha {

    public static void main(String[] args) throws InterruptedException {
        final String source = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890";
        List<Character> queue = new ArrayList<Character>();

        Thread genThread1 = new Thread(new Gen(source,queue));
        Thread genThread2 = new Thread(new Gen(source,queue));
        genThread1.setDaemon(true);
        genThread2.setDaemon(true);

        Thread builderThread1 = new Thread(new CaptchaBuilder(queue));
        Thread builderThread2 = new Thread(new CaptchaBuilder(queue));
        Thread builderThread3 = new Thread(new CaptchaBuilder(queue));
        Thread builderThread4 = new Thread(new CaptchaBuilder(queue));
        Thread builderThread5 = new Thread(new CaptchaBuilder(queue));

        genThread1.start();
        genThread2.start();

        builderThread1.start();
        builderThread2.start();
        builderThread3.start();
        builderThread4.start();
        builderThread5.start();
    }


    private static class Gen implements Runnable {
        private final String source;
        private final List<Character> queue;

        public Gen(String source, List<Character> queue) {
            this.source = source;
            this.queue = queue;
        }

        @Override
        public void run() {
            synchronized(queue) {
                while (true) {
                    if (queue.size() >= 50){
                        try {
                            queue.notify();
                            queue.wait();
                        } catch (InterruptedException ignore) {
                            /*NOP*/
                        }
                    } else {
                        int r = (int)(Math.random() * source.length());
                        queue.add(source.charAt(r));
                    }
                }
            }
        }
    }

    private static class CaptchaBuilder implements Runnable {
        private final List<Character> buf;
        private final int N = 100;
        private final int CAPTCHA_SIZE = 5;
        private int count;

        public CaptchaBuilder(List<Character> buf) {
            this.buf = buf;
            this.count = 0;
        }

        @Override
        public void run() {
            synchronized (buf) {
                while(count < N) {
                    System.out.println(buf.toString());
                    if (buf.size() < CAPTCHA_SIZE) {
                        buf.notify();
                        try {
                            buf.wait();
                        } catch (InterruptedException ignore) {/*NOP*/}
                    } else {
                        for(int i=0; i<CAPTCHA_SIZE; i++)
                            //System.out.print(buf.remove(buf.size() - 1));
                            System.out.print(buf.remove(0));
                        System.out.println();
                        count++;
                        buf.notify();
                        try {
                            buf.wait();
                            Thread.sleep(250);
                        } catch (InterruptedException ignore) {/*NOP*/}
                    }
                }
                buf.notify();
            }
        }
    }
}