import java.util.Arrays;

public class P76 {

	/**
	 * Finds the number of ways the given n can be written as a sum of at least
	 * two positive integers.
	 */
	public static int solve(int n) {
		// Let t[i][j] represent the number of ways i can be written as a sum of
		// positive integers, none of which is greater than j.
		int[][] t = new int[n + 1][];
		// Trivial cases.
		t[0] = new int[n + 1];
		Arrays.fill(t[0], 1);
		t[1] = new int[n + 1];
		Arrays.fill(t[1], 1);
		// Compute higher values from smaller ones.
		for (int i = 2; i <= n; i++) {
			t[i] = new int[n + 1];
			for (int j = 1; j <= n; j++) {
				// Either we take j as our greatest term, or we do not.
				int r = i - j;
				if (r >= 0) {
					t[i][j] = t[r][j];
				}
				t[i][j] += t[i][j - 1];
				// System.out.print(t[i][j] + " ");
			}
			// System.out.println();
		}
		return t[n][n - 1];
	}

	public static void main(String[] args) {
		Utils.timeThis(() -> {
			int n = 100;
			System.out.println("Found " + solve(n) + " sums for " + n);
		});
	}

}
