import java.math.BigInteger;
import java.util.Collection;

public class Utils {

	public static void timeThis(Runnable runnable) {
		long start = System.currentTimeMillis();
		try {
			runnable.run();
		} catch (ThreadDeath t) {
			long end = System.currentTimeMillis();
			System.out.printf("Killed after %.3fs.\n",
					(double) (end - start) / 1000);
			throw t;
		} catch (RuntimeException e) {
			long end = System.currentTimeMillis();
			System.out.printf("Died after %.3fs.\n",
					(double) (end - start) / 1000);
			throw e;
		}
		long end = System.currentTimeMillis();
		System.out.printf("Finished after %.3fs.\n",
				(double) (end - start) / 1000);
	}

	public static BigInteger sumInts(Collection<Integer> is) {
		BigInteger sum = BigInteger.ZERO;
		for (Integer i : is) {
			sum = sum.add(BigInteger.valueOf(i));
		}
		return sum;
	}
}
