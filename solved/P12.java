public class P12 {
	public static int numDivisors(int n) {
		int d = 1;
		for (int i = 2; i <= n; i++) {
			if (n % i == 0) {
				int a = 0;
				do {
					a++;
					n /= i;
				} while (n % i == 0);
				d *= a + 1;
			}
		}
		return d;
	}

	public static void main(String[] args) {
		/*
		for (int i = 1; i <= 20; i++) {
			int numDiv = numDivisors(i);
			System.out.printf("%9d %9d %9d\n", i, i, numDiv);
		}
		*/
		for (int i = 1; i <= 100000; i++) {
			int sum = i * (i + 1) / 2;
			int numDiv = numDivisors(sum);
			/*
			int numDiv = i % 2 == 0 ?
				(numDivisors(i/2) * numDivisors(i+1)) :
				(numDivisors(i) * numDivisors((i+1)/2));
			*/
			/*
			if (i % 100 == 0) {
				System.out.printf("%9d %9d %9d\n", i, sum, numDiv);
			}
			*/
			if (numDiv >= 500) {
				System.out.printf("%9d %9d %9d\n", i, sum, numDiv);
				break;
			}
		}
	}
}
