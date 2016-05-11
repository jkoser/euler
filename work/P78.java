import java.math.BigInteger;
import java.util.Arrays;

public class P78 {

	/**
	 * Finds the number of ways the given n can be written as a sum of positive
	 * integers.
	 */
	public static BigInteger solve(int n) {
		final BigInteger PRINT_INTERVAL = BigInteger.valueOf(10000);
		// Let t[i][j] represent the number of ways i can be written as a sum of
		// positive integers, none of which is greater than j.
		BigInteger[][] t = new BigInteger[n + 1][];
		// Trivial cases.
		t[0] = new BigInteger[n + 1];
		Arrays.fill(t[0], BigInteger.ONE);
		t[1] = new BigInteger[n + 1];
		Arrays.fill(t[1], BigInteger.ONE);
		// Compute higher values from smaller ones.
		for (int i = 2; i <= n; i++) {
			t[i] = new BigInteger[n + 1];
			t[i][0] = BigInteger.ZERO;
			for (int j = 1; j <= n; j++) {
				// Either we take j as our greatest term, or we do not.
				int r = i - j;
				if (r >= 0) {
					t[i][j] = t[r][j];
				} else {
					t[i][j] = BigInteger.ZERO;
				}
				t[i][j] = t[i][j].add(t[i][j - 1]);
				// System.out.print(t[i][j] + " ");
			}
			if (t[i][n].mod(PRINT_INTERVAL).equals(BigInteger.ZERO))
				System.out.printf("p(%d) = %d\n", i, t[i][n]);
		}
		return t[n][n];
	}

	public static void main(String[] args) {
		Utils.timeThis(() -> {
			int n = 10000;
			System.out.println("Found " + solve(n) + " sums for " + n);
		});
	}
}
