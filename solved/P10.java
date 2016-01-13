import java.math.BigInteger;
import java.util.BitSet;

public class P10 {
	private static final int MAX = 2000000;
	private static final BitSet bits = new BitSet(MAX);

	private static void custom() {
		int count = 0;
		BigInteger sum = BigInteger.ZERO;
		for (int i = 2; i < MAX; i = bits.nextClearBit(i + 1)) {
			count++;
			sum = sum.add(new BigInteger(String.valueOf(i)));
			for (long j = ((long) i) * i; j < MAX; j += i) {
				bits.set((int) j);
			}
		}
		System.out.println("count: " + count);
		System.out.println("sum: " + sum);
	}

	private static void sieve() {
		PrimeSieve sieve = new PrimeSieve(MAX);
		System.out.println("count: " + sieve.getList().size());
		System.out.println("sum: " + Utils.sumInts(sieve.getList()));
	}

	public static void main(String[] args) {
		Utils.timeThis(new Runnable() {
			@Override
			public void run() {
				custom();
			}
		});
		Utils.timeThis(new Runnable() {
			@Override
			public void run() {
				sieve();
			}
		});
	}
}
