public class P14 {
	private static final int CACHE_SIZE = 4000000;
	private static final int[] cache = new int[CACHE_SIZE];
	static { cache[1] = 1; }

	public static int collatz(long n) {
		if (n < CACHE_SIZE && cache[(int) n] > 0) {
			return cache[(int) n];
		} else {
			long next = n % 2 == 0 ? (n / 2) : (3 * n + 1);
			int length = 1 + collatz(next);
			if (n < CACHE_SIZE)
				cache[(int) n] = length;
			return length;
		}
	}

	public static void main(String[] args) {
		int maxLength = 0;
		long n = 0;
		for (long i = 1; i < 1000000; i++) {
			int length = collatz(i);
			if (length > maxLength) {
				n = i;
				maxLength = length;
			}
		}
		System.out.println("n: " + n + ", length: " + maxLength);
	}
}
