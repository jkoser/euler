import java.math.BigInteger;
import java.util.*;

/*
 * Consider quadratic Diophantine equations of the form:
 *
 * x^2 – Dy^2 = 1
 *
 * For example, when D=13, the minimal solution in x is 649^2 – 13×180^2 = 1.
 *
 * It can be assumed that there are no solutions in positive integers when D is
 * square.
 *
 * By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
 * following:
 *
 * 3^2 – 2×2^2 = 1
 * 2^2 – 3×1^2 = 1
 * 9^2 – 5×4^2 = 1
 * 5^2 – 6×2^2 = 1
 * 8^2 – 7×3^2 = 1
 *
 * Hence, by considering minimal solutions in x for D ≤ 7, the largest x is
 * obtained when D=5.
 *
 * Find the value of D ≤ 1000 in minimal solutions of x for which the largest
 * value of x is obtained.
 */
public class P66 {
    private static Set<Integer> squares = new HashSet<Integer>();

    public static int minXbrute(int d) {
        for (long x = 2; true; x++) {
            for (long y = 1; y < x; y++) {
                if (x * x - d * y * y == 1) {
                    return (int) x;
                }
            }
        }
    }

    public static int minX(int d) {
        long x = 2, y = 1;
        while (true) {
            long z = x * x - d * y * y;
            if (z == 1) {
                return (int) x;
            } else if (z < 1) {
                x++;
            } else {
                y++;
            }
        }
    }

    /*
    function extended_gcd(a, b)
        x := 0    lastx := 1
        y := 1    lasty := 0
        while b ≠ 0
            quotient := a div b
            (a, b) := (b, a mod b)
            (x, lastx) := (lastx - quotient*x, x)
            (y, lasty) := (lasty - quotient*y, y)       
        return (lastx, lasty)
     */
    public static long modMultInverse(long a, long m) {
        long aa = m;
        long bb = a % m;
        long y = 1, lasty = 0;
        while (bb != 0) {
            //System.out.prlongln("aa: " + aa + " bb: " + bb + " y: " + y);
            long q = aa / bb;
            long t = bb;
            bb = aa % bb;
            aa = t;
            t = lasty - q * y;
            lasty = y;
            y = t;
        }
        //System.out.println("aa: " + aa + " bb: " + bb + " y: " + y);
        if (aa == 1) {
            return lasty;
        } else {
            throw new IllegalArgumentException(a + " has no inverse mod " + m);
        }
    }

    public static long chakravala(int d) {
        long a = (long) Math.round(Math.sqrt(d));
        long b = 1;
        long k = a * a - d;
        while (k != 1) {
            long absk = Math.abs(k);
            // want m > 0 such that k | a + bm and abs(m * m - d) is minimal
            // (a + bm) mod k == 0
            // m = - a * b^-1
            long m = (- a * modMultInverse(b, absk)) % absk;
            if (m <= 0) {
                m += absk;
            }
            long best = Math.abs(m * m - d);
            while (true) {
                long m1 = m + absk;
                long z = Math.abs(m1 * m1 - d);
                if (z < best) {
                    z = best;
                    m = m1;
                } else {
                    break;
                }
            }
            //System.out.println("a: " + a + " b: " + b + " k: " + k + " m: " + m);
            long anew = (a * m + d * b) / absk;
            long bnew = (a + b * m) / absk;
            long knew = (m * m - d) / k;
            a = anew;
            b = bnew;
            k = knew;
        }
        //System.out.println("a: " + a + " b: " + b + " k: " + k);
        return a;
    }

    public static BigInteger chakravalaBig(int d) {
        int sqrtD = (int) Math.round(Math.sqrt(d));
        BigInteger a = BigInteger.valueOf(sqrtD);
        BigInteger b = BigInteger.ONE;
        BigInteger bigD = BigInteger.valueOf(d);
        BigInteger k = a.pow(2).subtract(bigD);
        while (!k.equals(BigInteger.ONE)) {
            BigInteger absk = k.abs();
            // want m > 0 such that k | a + bm and abs(m * m - d) is minimal
            // (a + bm) mod k == 0
            // m = - a * b^-1
            BigInteger m = (a.negate().multiply(b.modInverse(absk))).mod(absk);
            if (m.compareTo(BigInteger.ZERO) <= 0) {
                m = m.add(absk);
            }
            BigInteger best = m.pow(2).subtract(bigD).abs();
            while (true) {
                BigInteger m1 = m.add(absk);
                BigInteger z = m1.pow(2).subtract(bigD).abs();
                if (z.compareTo(best) < 0) {
                    z = best;
                    m = m1;
                } else {
                    break;
                }
            }
            //System.out.println("a: " + a + " b: " + b + " k: " + k + " m: " + m);
            BigInteger anew = a.multiply(m).add(bigD.multiply(b)).divide(absk);
            BigInteger bnew = a.add(b.multiply(m)).divide(absk);
            BigInteger knew = m.pow(2).subtract(bigD).divide(k);
            a = anew;
            b = bnew;
            k = knew;
        }
        //System.out.println("a: " + a + " b: " + b + " k: " + k);
        return a;
    }

    public static void main(String[] args) {
        int limit = Integer.parseInt(args[0]);
        int n = 1;
        int s = 1;
        while (s <= limit) {
            squares.add(s);
            n++;
            s = n * n;
        }
        BigInteger bestx = BigInteger.ZERO;
        int bestd = 0;
        for (int d = 2; d <= limit; d++) {
            if (!squares.contains(d)) {
                //System.out.println(d + " - " + chakravala(d) + " - " + minX(d));
                BigInteger x = chakravalaBig(d);
                if (x.compareTo(bestx) > 0) {
                    bestx = x;
                    bestd = d;
                }
            }
        }
        System.out.println("bestx: " + bestx + " bestd: " + bestd);
    }
}
