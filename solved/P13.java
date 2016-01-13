import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigInteger;

public class P13 {
	public static void main(String[] args) throws IOException {
		String fileName = "/home/justin/src/euler/solved/p13.txt";
		BufferedReader in = new BufferedReader(new FileReader(fileName));
		BigInteger sum = BigInteger.ZERO;
		for (int i = 0; i < 100; i++) {
			String line = in.readLine();
			BigInteger n = new BigInteger(line);
			sum = sum.add(n);
		}
		in.close();
		String s = sum.toString();
		System.out.println(s.substring(0, 10));
	}
}
