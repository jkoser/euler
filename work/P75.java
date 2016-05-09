import java.util.HashSet;
import java.util.Set;

import com.google.common.base.Strings;

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

	public static void analyzeSquares() {
		int bits = 4;
		int n = 1 << bits;
		Set<Integer> squareEnds = new HashSet<>();
		for (int i = 0; i < n; i++) {
			int iEnd = i & (n - 1);
			int iSquaredEnd = (i * i) & (n - 1);
			System.out.printf("%s  %s\n", Strings.padStart(Integer.toBinaryString(iEnd), bits, '0'),
					Strings.padStart(Integer.toBinaryString(iSquaredEnd), bits, '0'));
			squareEnds.add(iSquaredEnd);
		}
		System.out.println("--");
		for (int q : squareEnds) {
			System.out.println(Strings.padStart(Integer.toBinaryString(q), bits, '0'));
		}
		System.out.println("--");
		Set<Integer> sumEnds = new HashSet<>();
		for (int q : squareEnds) {
			for (int r : squareEnds) {
				sumEnds.add((q + r) & (n - 1));
			}
		}
		for (int s : sumEnds) {
			System.out.println(Strings.padStart(Integer.toBinaryString(s), bits, '0'));
		}
	}

	public static void main(String[] args) {
		analyzeSquares();
	}

}
