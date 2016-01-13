import java.math.BigInteger;

import com.google.common.math.BigIntegerMath;

public class P15 {
	public static long fact(int n) {
		long p = 1;
		for (int i = 2; i <= n; i++) {
			p *= i;
		}
		return p;
	}

	public static long choose(int n, int k) {
		return fact(n) / fact(k) / fact(n - k);
	}

	public static void main(String[] args) {
		Utils.timeThis(new Runnable() {
			@Override
			public void run() {
				long sum = 0;
				for (int i = 0; i <= 20; i++) {
					long c = choose(20, i);
					sum += c * c;
				}
				System.out.println(sum);
			}
		});
		// Preload library
		BigIntegerMath.binomial(1, 0);
		Utils.timeThis(new Runnable() {
			@Override
			public void run() {
				BigInteger sum = BigInteger.ZERO;
				for (int i = 0; i <= 20; i++) {
					BigInteger b = BigIntegerMath.binomial(20, i);
					sum = sum.add(b.multiply(b));
				}
				System.out.println(sum);
			}
		});
	}
}
