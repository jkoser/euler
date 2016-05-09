
public class P75 {

	public static int iterateOverSideLengths(int limit) {
		int[] numTriangles = new int[limit + 1];
		for (long a = 3; a < (int) (limit / (2 + Math.sqrt(2))) + 1; a++) {
			for (long b = a; b < (limit - a) / 2; b++) {
				// System.out.println(a + ", " + b);
				if (Utils.isSquare(a * a + b * b)) {
					int c = (int) Math.sqrt(a * a + b * b);
					int p = (int) (a + b + c);
					if (p <= limit) {
						// System.out.printf("%20s%8d\n", a + ", " + b + ", " +
						// c, p);
						numTriangles[p]++;
					}
				}
			}
		}
		int numSingles = 0;
		for (int p = 0; p <= limit; p++) {
			// System.out.printf("%10d%10d\n", p, numTriangles[p]);
			if (numTriangles[p] == 1)
				numSingles++;
		}
		return numSingles;
	}

	public static void solve() {
		Utils.timeThis(() -> {
			int limit = 1500000;
			System.out.printf("%d perimeters with unique triangles up to %d\n", iterateOverSideLengths(limit), limit);
		});
	}

	public static void main(String[] args) {
		solve();
	}

}
