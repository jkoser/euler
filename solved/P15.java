public class P15 {
	public static long fact(int n) {
		long p = 1;
		for (int i = 2; i <= n; i++) {
			p *= i;
		}
		return p;
	}

	public static long choose(int n, int k) {
		return fact(n) / fact(k) / fact(n - k);
	}

	public static void main(String[] args) {
		long sum = 0;
		for (int i = 0; i <= 20; i++) {
			long c = choose(20, i);
			sum += c * c;
		}
		System.out.println(sum);
	}
}
