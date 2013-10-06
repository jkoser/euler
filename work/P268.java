import java.util.ArrayList;
import java.util.List;

/**
 * It can be verified that there are 23 positive integers less than 1000 that
 * are divisible by at least four distinct primes less than 100.
 * 
 * Find how many positive integers less than 10^16 are divisible by at least
 * four distinct primes less than 100.
 * 
 * <pre>
 * f(10^3) = 23
 * f(10^4) = 811
 * f(10^5) = 9280
 * f(10^6) = 77579
 * f(10^7) = 768778
 * 
 * (for f(10^16), use at most 13 distinct primes)
 * 
 * How many < 300 divisible by two < 10?
 * 
 * 2 * 3 -> 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96,
 *          102, 108, 114, 120, 126, 132, 138, 144, 150, 156, 162, 168, 174,
 *          180, 186, 192, 198, 204, 210, 216, 222, 228, 234, 240, 246, 252,
 *          258, 264, 270, 276, 282, 288, 294
 * 2 * 5 -> 10, 20, *30, 40, 50, *60, 70, 80, *90, 100, 110, *120, 130, 140,
 *          *150, 160, 170, *180, 190, 200, *210, 220, 230, *240, 250, 260,
 *          *270, 280, 290
 * 2 * 7 -> 14, 28, *42, 56, *70, *84, 98, 112, *126, *140, 154, *168, 182,
 *          196, **210, 224, 238, *252, 266, *280, *294
 * 3 * 5 -> 15, **30, 45, **60, 75, **90, 105, **120, 135, **150, 165, **180,
 *          195, ***210, 225, **240, 255, **270, 285
 * 3 * 7 -> 21, **42, 63, **84, *105, **126, 147, **168, 189, ****210, 231,
 *          **252, 273, **294
 * 5 * 7 -> 35, **70, **105, **140, 175, *****210, 
 * 
 * p1^a * p2^b * ... (pk > p2)
 * 
 * How many < 10^16 divisible by one < 10?
 * 
 * {2k} +
 * {3k} - {6k} +
 * {5k} - ({10k} + {15k} - {30k}) +
 * {7k} - ({14k} + {21k} + {35k} - ...
 * 
 * (x,0,0,0) - _log2(n)_
 * (0,x,0,0) - _log3(n)_
 * (0,0,x,0) - _log5(n)_
 * 
 * (a,b,c,d) s.t. at least one nonzero and
 *                2^a * 3^b * 5^c * 7^d * ... < 10^16
 *                a*log(2) + b*log(3) + c*log(5) + d*log(7) + ... < 16
 *                a + b*log2(3) + c*log2(5) + d*log2(7) + ... < log2(10^16)
 * 
 * </pre>
 */
public class P268 {
    public static int PRIME_LIMIT = 100;
    public static long LIMIT = 1000;

    public static long answer;

    public static void main(String[] args) {
        Utils.timeThis(new Runnable() {
            @Override
            public void run() {
                int c = 0;
                for (long i = 1; i < LIMIT; i++) {
                    if (isNotable(i)) {
                        // System.out.println(i);
                        c++;
                    }
                }
                answer = c;
            }
        });
        System.out.println(LIMIT + ", " + PRIME_LIMIT + ", " + answer);
    }

    public static boolean isNotable(long n) {
        int distinct = 0;
        if (n % 2 == 0) {
            distinct = 1;
            do {
                n /= 2;
            } while (n % 2 == 0);
        }
        int k = 3;
        while (k * k <= n && k < PRIME_LIMIT) {
            if (n % k == 0) {
                distinct++;
                do {
                    n /= k;
                } while (n % k == 0);
            }
            k += 2;
        }
        if (1 < n && n < PRIME_LIMIT) distinct++;
        return distinct >= 4;
    }

    public static long badEstimate() {
        PrimeSieve primes = new PrimeSieve(PRIME_LIMIT);
        List<Long> combinations = new ArrayList<Long>();
        long total = 0L;
        for (int p1 : primes.getList()) {
            for (int p2 : primes.getList()) {
                if (p2 <= p1) continue;
                for (int p3 : primes.getList()) {
                    if (p3 <= p2) continue;
                    for (int p4 : primes.getList()) {
                        if (p4 <= p3) continue;
                        long n = (long) p1 * p2 * p3 * p4;
                        combinations.add(n);
                        if (n < LIMIT) {
                            // System.out.println(p1+" "+p2+" "+p3+" "+p4+" "+
                            // n);
                            long m = LIMIT / n;
                            // System.out.println(m);
                            total += m;
                        }
                    }
                }
            }
        }
        System.out.println("combinations = " + combinations.size());
        System.out.println("total + dups = " + total);
        for (int i = 0; i < combinations.size(); i++) {
            for (int j = 0; j < i; j++) {
                total -= LIMIT / (combinations.get(i) * combinations.get(j));
            }
        }
        System.out.println("total - dups = " + total);
        return total;
    }
}
