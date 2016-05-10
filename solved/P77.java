import java.util.Arrays;
import java.util.List;

public class P77 {

	/**
	 * Finds the smallest integer that can be written as a sum of primes in at
	 * least the given number of ways.
	 */
	public static int solve(int numWays) {
		// Worst case, we exchange (2 + 2 + 2) for (3 + 3).
		int limit = numWays * 6 + 1;
		PrimeSieve sieve = new PrimeSieve(limit);
		List<Integer> primes = sieve.getList();
		// Let t[i][j] represent the number of ways i can be written as a sum of
		// primes, none of which is greater than primes[j].
		int[][] t = new int[limit][];
		// Trivial cases.
		t[0] = new int[primes.size()];
		Arrays.fill(t[0], 1);
		t[1] = new int[primes.size()];
		Arrays.fill(t[1], 0);
		// Compute higher values from smaller ones.
		for (int i = 2; i <= limit; i++) {
			t[i] = new int[primes.size()];
			for (int j = 0; j < primes.size(); j++) {
				// Either we take primes[j] as our greatest term, or we do not.
				int r = i - primes.get(j);
				if (r >= 0) {
					t[i][j] = t[r][j];
				}
				if (j > 0) {
					t[i][j] += t[i][j - 1];
				}
				// System.out.print(t[i][j] + " ");
			}
			// System.out.println();
			if (t[i][primes.size() - 1] >= numWays)
				return i;
		}
		// This will never be reached.
		assert false;
		return 0;
	}

	public static void main(String[] args) {
		Utils.timeThis(() -> {
			int numWays = 5000;
			System.out.println("Smallest number with " + numWays + " prime sums is " + solve(numWays));
		});
	}

}
