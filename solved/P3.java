public class P3 {
	public static long findFactor(long n, long start) {
		for (long i = start; i * i <= n; i += 2) {
			if (n % i == 0)
				return i;
		}
		return n;
	}

	public static void main(String[] args) {
		long n = 600851475143L;
		long i = 3;
		do {
			i = findFactor(n, i);
			n /= i;
			System.out.println("i=" + i + ", n=" + n);
		} while (n > 1);
	}
}
