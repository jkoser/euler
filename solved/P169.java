import java.math.BigInteger;

/**
 * Define f(0)=1 and f(n) to be the number of different ways n can be expressed
 * as a sum of integer powers of 2 using each power no more than twice.
 * <p>
 * For example, f(10)=5 since there are five different ways to express 10:
 * 
 * <pre>
 * 1 + 1 + 8
 * 1 + 1 + 4 + 4
 * 1 + 1 + 2 + 2 + 4
 * 2 + 4 + 4
 * 2 + 8
 * 
 * 10 = 0b1010 = 10 + 0 = 8 + 2
 *        1002 =  9 + 1
 *        0202 =  5 + 5
 *        0122 =  7 + 3
 *        0210 =  6 + 4
 *        
 * 8 = 0b1000
 *       0200
 *       0120
 *       0112
 * </pre>
 * 
 * What is f(10^25)?
 */
public class P169 {

    public static int fBruteForceHelper(long n, long highBit, boolean carry) {
        if (highBit == 0) {
            return carry ? 0 : 1;
        } else if ((n & highBit) != 0) {
            if (carry) {
                // must push the carry down
                return fBruteForceHelper(n, highBit >>> 1, true);
            } else {
                // may carry
                return fBruteForceHelper(n, highBit >>> 1, true) +
                        fBruteForceHelper(n, highBit >>> 1, false);
            }
        } else {
            if (carry) {
                // may carry
                return fBruteForceHelper(n, highBit >>> 1, true) +
                        fBruteForceHelper(n, highBit >>> 1, false);
            } else {
                // nothing to push
                return fBruteForceHelper(n, highBit >>> 1, false);
            }
        }
    }

    /**
     * Calculates f(n) by counting all configurations.
     */
    public static int fBruteForce(long n) {
        return fBruteForceHelper(n, Long.highestOneBit(n), false);
    }

    /**
     * Calculates f(n) in a dynamic programming style by tracking the number of
     * configurations for the low bits and progressively considering higher
     * bits.
     */
    public static BigInteger fDynamic(BigInteger n) {
        BigInteger fCarry = BigInteger.ZERO;
        BigInteger fNoCarry = BigInteger.ONE;
        while (!n.equals(BigInteger.ZERO)) {
            if (n.and(BigInteger.ONE).equals(BigInteger.ONE)) {
                BigInteger t = fCarry;
                fNoCarry = fNoCarry.add(fCarry);
                fCarry = t;
            } else {
                BigInteger t = fCarry.add(fNoCarry);
                // fNoCarry = fNoCarry;
                fCarry = t;
            }
            n = n.shiftRight(1);
        }
        return fNoCarry;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        BigInteger n = BigInteger.ONE;
        for (int i = 0; i <= 25; i++) {
            System.out.println("f(" + n + ") = " + fDynamic(n));
            n = n.multiply(BigInteger.TEN);
        }
        long startTime = System.currentTimeMillis();
        n = BigInteger.ONE;
        for (int i = 0; i < 25000; i++) {
            n = n.multiply(BigInteger.TEN);
        }
        fDynamic(n);
        long endTime = System.currentTimeMillis();
        System.out.println(endTime - startTime);
    }

}
