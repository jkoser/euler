public class NthPrime {
	// O(sqrt(x))
	public static boolean isPrime(long x) {
		for (long i = 2; i * i <= x; i++) {
			if (x % i == 0)
				return false;
		}
		return true;
	}

	// O(e^n * sqrt(n)) ?
	public static long testEach(int n) {
		long p = 2;
		int c = 0;
		for (int i = 2; c < n; i++) {
			if (isPrime(i)) {
				p = i;
				c++;
			}
		}
		return p;
	}

	public static void main(String[] args) {
		int n = Integer.valueOf(args[0]);
		long start = System.currentTimeMillis();
		long p = testEach(n);
		System.out.println(p);
		long stop = System.currentTimeMillis();
		double seconds = (stop - start) / 1000.0;
		System.out.printf("%.1f seconds\n", seconds);
	}
}
