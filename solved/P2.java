public class P2 {
	public static void main(String[] args) {
		long sum = 0;
		int a = 1, b = 1, c;
		while (b < 4000000) {
			if (b % 2 == 0)
				sum += b;
			c = a + b;
			a = b;
			b = c;
		}
		System.out.println(sum);
	}
}
