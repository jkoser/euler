public class P6 {
	public static void main(String[] args) {
		long sumSquares = 0;
		for (int i = 1; i <= 100; i++)
			sumSquares += i * i;
		int sum = 100 * 101 / 2;
		System.out.println(sum * sum - sumSquares);
	}
}
