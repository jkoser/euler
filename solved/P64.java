import java.util.*;

public class P64 {
    private static Set<Integer> squares = new HashSet<Integer>();

    public static int gcd(int a, int b) {
        if (a < 1 || b < 1) {
            throw new IllegalArgumentException();
        }
        if (a < b) {
            int t = a;
            a = b;
            b = t;
        }
        while (b > 0) {
            int r = a % b;
            a = b;
            b = r;
        }
        return a;
    }

    public static int periodLength(int n) {
        double sqrt = Math.sqrt(n);
        int a0 = (int) Math.floor(sqrt);
        int p0 = 1;
        int q0 = a0;
        int i = 0;
        int p = p0, q = q0;
        do {
            int r = n - q * q;
            int a = (int) Math.floor(p * (sqrt + q) / r);
            p = r / p;
            q = - (q - a * p);
            i++;
        } while (!(p == p0 && q == q0));
        return i;
    }

    public static void main(String[] args) {
        int limit = 10000;
        if (args.length > 0) {
            limit = Integer.parseInt(args[0]);
        }
        int n = 1;
        int s = 1;
        while (s <= limit) {
            squares.add(s);
            n++;
            s = n * n;
        }
        int c = 0;
        for (n = 2; n <= limit; n++) {
            if (!squares.contains(n) && periodLength(n) % 2 == 1) {
                c++;
            }
        }
        System.out.println(c);
    }
}
