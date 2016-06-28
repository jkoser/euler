import java.util.ArrayList;

public class P78 {

	/**
	 * Finds the least n such that the number of partitions p(n) ~= 0 (mod m)
	 * for the given m.
	 */
	public static int solve(int m) {
		ArrayList<Integer> pn = new ArrayList<>();
		pn.add(1);
		pn.add(1);
		for (int n = 2;; n++) {
			int p = 0;
			int s = -1;
			// loop through generalized pentagonal numbers to calculate p(n) by
			// recurrence
			for (int i = 1;; i = (i > 0 ? -i : -i + 1)) {
				int j = n - i * (3 * i - 1) / 2;
				if (j < 0) {
					break;
				}
				if (i > 0) {
					s = -s;
				}
				p = (p + s * pn.get(j)) % m;
			}
			if (p == 0) {
				return n;
			}
			pn.add(p);
		}
	}

	public static void main(String[] args) {
		Utils.timeThis(() -> {
			int m = 1000000;
			System.out.println(solve(m) + " is least n where p(n) = 0 mod " + m);
		});
	}
}
