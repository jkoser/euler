import java.io.*;
import java.util.Random;

public class P11 {
	public static final int DIM = 20;
	public static final int MAX_CELL = 100;
	public static final String CELL_FORMAT = "%02d";
	public static final int RUN = 4;
	public static final Random RAND = new Random();

	public static int[][] generateRandomMatrix() {
		int[][] m = new int[DIM][];
		for (int i = 0; i < DIM; i++) {
			m[i] = new int[DIM];
			for (int j = 0; j < DIM; j++) {
				m[i][j] = RAND.nextInt(MAX_CELL);
			}
		}
		return m;
	}

	public static int[][] readMatrix() throws IOException {
		BufferedReader in =
			new BufferedReader(new InputStreamReader(System.in));
		int[][] m = new int[DIM][];
		for (int i = 0; i < DIM; i++) {
			String line = in.readLine();
			String[] ss = line.split(" ");
			m[i] = new int[DIM];
			for (int j = 0; j < DIM; j++) {
				m[i][j] = Integer.parseInt(ss[j]);
			}
		}
		return m;
	}

	public static void printMatrix(int[][] m) {
		for (int i = 0; i < m.length; i++) {
			for (int j = 0; j < m[i].length; j++) {
				if (j != 0)
					System.out.print(" ");
				System.out.printf(CELL_FORMAT, m[i][j]);
			}
			System.out.println();
		}
	}

	public static int greatestRunProduct(int[][] m) {
		int max = 0;
		for (int i = 0; i <= DIM - RUN; i++) {
			for (int j = 0; j <= DIM - RUN; j++) {
				// down
				int p = 1;
				for (int k = 0; k < RUN; k++)
					p *= m[i+k][j];
				max = Math.max(max, p);
				// right
				p = 1;
				for (int k = 0; k < RUN; k++)
					p *= m[i][j+k];
				max = Math.max(max, p);
				// diagonal
				p = 1;
				for (int k = 0; k < RUN; k++)
					p *= m[i+k][j+k];
				max = Math.max(max, p);
			}
		}
		for (int i = RUN - 1; i < DIM; i++) {
			for (int j = 0; j <= DIM - RUN; j++) {
				// other diagonal
				int p = 1;
				for (int k = 0; k < RUN; k++)
					p *= m[i-k][j+k];
				max = Math.max(max, p);
			}
		}
		return max;
	}

	public static void main(String [] args) throws IOException {
		int[][] m = readMatrix();
		printMatrix(m);
		System.out.println(greatestRunProduct(m));
	}
}
