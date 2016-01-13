import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

public class P8 {
	public static byte[] readDigits() throws FileNotFoundException {
		String fileName = "/home/justin/src/euler/solved/p8.txt";
		BufferedReader in = new BufferedReader(new FileReader(fileName));
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
		} catch (IOException e) {
		}
		try {
			in.close();
		} catch (IOException e) {
		}
		return digits;
	}

	public static void main(String[] args) throws FileNotFoundException {
		byte[] digits = readDigits();
		long maxProduct = 0;
		for (int i = 0; i < 988; i++) {
			System.out.print(digits[i]);
			long p = 1;
			for (int k = 0; k < 13; k++) {
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
