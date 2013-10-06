import java.io.*;
import java.math.BigInteger;

public class P13 {
	public static void main(String[] args) throws IOException {
		BufferedReader in =
			new BufferedReader(new InputStreamReader(System.in));
		BigInteger sum = BigInteger.ZERO;
		for (int i = 0; i < 100; i++) {
			String line = in.readLine();
			BigInteger n = new BigInteger(line);
			sum = sum.add(n);
		}
		String s = sum.toString();
		System.out.println(s.substring(0, 10));
	}
}
