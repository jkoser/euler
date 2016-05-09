import java.math.BigInteger;
import java.util.Collection;

public class Utils {

	public static void timeThis(Runnable runnable) {
		long start = System.currentTimeMillis();
		try {
			runnable.run();
		} catch (ThreadDeath t) {
			long end = System.currentTimeMillis();
			System.out.printf("Killed after %.3fs.\n", (double) (end - start) / 1000);
			throw t;
		} catch (RuntimeException e) {
			long end = System.currentTimeMillis();
			System.out.printf("Died after %.3fs.\n", (double) (end - start) / 1000);
			throw e;
		}
		long end = System.currentTimeMillis();
		System.out.printf("Finished after %.3fs.\n", (double) (end - start) / 1000);
	}

	public static BigInteger sumInts(Collection<Integer> is) {
		BigInteger sum = BigInteger.ZERO;
		for (Integer i : is) {
			sum = sum.add(BigInteger.valueOf(i));
		}
		return sum;
	}

	/**
	 * Square testing from StackOverflow. http://stackoverflow.com/a/18686659
	 */
	private static long goodMask; // 0xC840C04048404040 computed below
	static {
		for (int i = 0; i < 64; ++i)
			goodMask |= Long.MIN_VALUE >>> (i * i);
	}

	public static boolean isSquare(long x) {
		// This tests if the 6 least significant bits are right.
		// Moving the to be tested bit to the highest position saves us masking.
		if (goodMask << x >= 0)
			return false;
		final int numberOfTrailingZeros = Long.numberOfTrailingZeros(x);
		// Each square ends with an even number of zeros.
		if ((numberOfTrailingZeros & 1) != 0)
			return false;
		x >>= numberOfTrailingZeros;
		// Now x is either 0 or odd.
		// In binary each odd square ends with 001.
		// Postpone the sign test until now; handle zero in the branch.
		if ((x & 7) != 1 | x <= 0)
			return x == 0;
		// Do it in the classical way.
		// The correctness is not trivial as the conversion from long to double
		// is lossy!
		final long tst = (long) Math.sqrt(x);
		return tst * tst == x;
	}
}
