/**
 * A particular school offers cash rewards to children with good attendance and
 * punctuality. If they are absent for three consecutive days or late on more
 * than one occasion then they forfeit their prize.
 * <p>
 * During an n-day period a trinary string is formed for each child consisting
 * of L's (late), O's (on time), and A's (absent).
 * <p>
 * Although there are eighty-one trinary strings for a 4-day period that can be
 * formed, exactly forty-three strings would lead to a prize:
 * 
 * <pre>
 * OOOO OOOA OOOL OOAO OOAA OOAL OOLO OOLA OAOO OAOA OAOL OAAO OAAL OALO OALA
 * OLOO OLOA OLAO OLAA AOOO AOOA AOOL AOAO AOAA AOAL AOLO AOLA AAOO AAOA AAOL
 * AALO AALA ALOO ALOA ALAO ALAA LOOO LOOA LOAO LOAA LAOO LAOA LAAO
 * </pre>
 * 
 * How many "prize" strings exist over a 30-day period?
 */
public class P191 {

    public static final int PERIOD = 30;

    // @formatter:off
    /*
     * Let Prize(d, a) denote the number of d-length prize strings with a (0
     * or 1) late days.
     * 
     * Let Absent(d, a) denote the number of d-length strings containing "AAA"
     * and a (0 or 1) late days.
     * 
     * Let Late(d) denote the number of d-length strings containing 2 or more
     * late days.
     * 
     * 3^d = Prize(d, 0) + Prize(d, 1) + Absent(d, 0) + Absent(d, 1) + Late(d)
     * 
     * d | 3^d | P(d,0) | P(d,1) | A(d,0) | A(d,1) | L(d)
     * --------------------------------------------------
     * 0 |   1 |      1 |      0 |      0 |      0 |    0
     * 1 |   3 |      2 |      1 |      0 |      0 |    0
     * 2 |   9 |      4 |      4 |      0 |      0 |    1
     * 3 |  27 |      7 |     12 |      1 |      0 |    7
     * 
     * L(d) = 3^d - 2^d - d * 2^(d-1)
     * 
     * sum over beginnings of first "AAA":
     * A(d,0) = P(d-3,0) + A(d-3,0) +                          // k = 0
     *   sum(k,1,d-3) { P(k-1,0) * (P(d-k-3,0) + A(d-k-3,0)) } // k > 0
     * A(d,1) = P(d-3,1) + A(d-3,1) +                  // k = 0
     *   sum(k,1,d-3) {                                // k > 0
     *     P(k-1,1) * (P(d-k-3,0) + A(d-k-3,0)) +      // late before k - 1
     *     P(k-1,0) * (P(d-k-3,0) + A(d-k-3,0)) +      // late at k - 1
     *     P(k-1,0) * (P(d-k-3,1) + A(d-k-3,1)) }      // late after k + 2
     * 
     * P(d,0) = 2^d - A(d,0)
     * P(d,1) = d * 2^(d-1) - A(d,1)
     */
    // @formatter:on

    /**
     * Solves by dynamic programming using formulas above.
     */
    public static long count(int days) {
        long[] P0 = new long[days + 1];
        long[] P1 = new long[days + 1];
        long[] A0 = new long[days + 1];
        long[] A1 = new long[days + 1];
        P0[0] = 1;
        P0[1] = 2;
        P1[1] = 1;
        P0[2] = 4;
        P1[2] = 4;
        P0[3] = 7;
        P1[3] = 12;
        A0[3] = 1;
        for (int d = 3; d <= days; d++) {
            A0[d] = P0[d - 3] + A0[d - 3];
            A1[d] = P1[d - 3] + A1[d - 3];
            for (int k = 1; k <= d - 3; k++) {
                A0[d] += P0[k - 1] * (P0[d - k - 3] + A0[d - k - 3]);
                A1[d] += P1[k - 1] * (P0[d - k - 3] + A0[d - k - 3]);
                A1[d] += P0[k - 1] * (P0[d - k - 3] + A0[d - k - 3]);
                A1[d] += P0[k - 1] * (P1[d - k - 3] + A1[d - k - 3]);
            }
            P0[d] = (1L << d) - A0[d];
            P1[d] = d * (1L << (d - 1)) - A1[d];
        }
        return P0[days] + P1[days];
    }

    public static final long[] POWERS_OF_THREE = new long[31];
    static {
        long p = 1;
        for (int i = 0; i < POWERS_OF_THREE.length; i++, p *= 3) {
            POWERS_OF_THREE[i] = p;
        }
    }

    public static final int T_LATE = 0;
    public static final int T_ON_TIME = 1;
    public static final int T_ABSENT = 2;

    /**
     * Represent an attendance string as a ternary number where 0 = late, 1 = on
     * time, 2 = absent. Returns true if the given string is a prize string.
     */
    public static boolean isPrize(long t) {
        boolean late = false;
        int a = 0;
        for (int i = 0; i < PERIOD; i++) {
            int d = (int) (t % 3);
            if (d == T_LATE) {
                if (late) return false;
                late = true;
                a = 0;
            } else if (d == T_ABSENT) {
                if (a == 2) return false;
                a++;
            } else {
                a = 0;
            }
            t /= 3;
        }
        return true;
    }

    public static void bruteForce() {
        int c = 0;
        for (long t = 0; t < POWERS_OF_THREE[PERIOD]; t++) {
            if (isPrize(t)) c++;
        }
        System.out.printf("%d prize strings of length %d\n", c, PERIOD);
    }

    private static long answer = 0;

    /**
     * @param args
     */
    public static void main(String[] args) {
        Utils.timeThis(new Runnable() {
            public void run() {
                answer = count(PERIOD);
            }
        });
        System.out.printf("%d prize strings of length %d\n", answer, PERIOD);
    }

}
