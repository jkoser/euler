import java.io.*;

public class P8 {
	public static byte[] readDigits() {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		byte[] digits = new byte[1000];
		int n = 0;
		String line = null;
		try {
			while ((line = in.readLine()) != null) {
				for (int i = 0; i < line.length(); i++) {
					digits[n + i] = Byte.parseByte(line.substring(i, i + 1));
				}
				n += line.length();
			}
		} catch (IOException e) { }
		return digits;
	}

	public static void main(String[] args) {
		byte[] digits = readDigits();
		int maxProduct = 0;
		for (int i = 0; i < 996; i++) {
			System.out.print(digits[i]);
			int p = 1;
			for (int k = 0; k < 5; k++) {
				p *= digits[i + k];
			}
			if (p > maxProduct) {
				maxProduct = p;
			}
		}
		System.out.println();
		System.out.println(maxProduct);
	}
}
