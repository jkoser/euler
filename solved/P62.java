import java.util.ArrayList;
import java.util.List;

public class P62 {

	/**
	 * Counts the number of digits from 0 to 9 in the given number, producing a
	 * long value in which bits 0..4 record the number of 0 digits, bits 5..9
	 * record the number of 1 digits, and so on.
	 * 
	 * @param n
	 *            non-negative integer
	 * @return a summary of the digits in n
	 */
	public static long digitCounts(long n) {
		long c = 0;
		while (n > 0) {
			int d = (int) (n % 10);
			c += 1L << (d * 5);
			n /= 10;
		}
		return c;
	}

	public static void solve(int numPermutations) {
		List<Long> cubes = new ArrayList<>();
		long limit = 10;
		for (long i = 1; true; i++) {
			long cube = i * i * i;
			if (cube >= limit) {
				// check list of all cubes with k digits
				for (long a : cubes) {
					int matches = 0;
					for (long b : cubes) {
						if (digitCounts(a) == digitCounts(b)) {
							matches++;
						}
					}
					if (matches >= numPermutations) {
						System.out.println(a);
						return;
					}
				}
				// reset for next round
				cubes.clear();
				limit *= 10;
			}
			cubes.add(cube);
		}
	}

	public static void main(String[] args) {
		Utils.timeThis(new Runnable() {
			@Override
			public void run() {
				solve(5);
			}
		});
	}

}
