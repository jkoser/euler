import java.math.BigDecimal;
import java.math.RoundingMode;

public class P80 {

	/**
	 * Computes the square root of the given x >= 1 to the given precision,
	 * rounding down.
	 */
	public static BigDecimal sqrt(BigDecimal x, int precision) {
		if (x.compareTo(BigDecimal.ONE) < 0) {
			throw new IllegalArgumentException("input must not be less than one");
		}
		BigDecimal low = BigDecimal.ONE.setScale(precision);
		BigDecimal high = x.setScale(precision);
		BigDecimal eps = BigDecimal.valueOf(1, precision);
		BigDecimal two = BigDecimal.valueOf(2);
		while (low.add(eps).pow(2).compareTo(x) <= 0) {
			BigDecimal mid = low.add(high).divide(two, precision, RoundingMode.CEILING);
			if (mid.pow(2).compareTo(x) <= 0) {
				low = mid;
			} else {
				high = mid;
			}
		}
		return low;
	}

	public static void main(String[] args) {
		int sum = 0;
		for (int i = 2; i <= 100; i++) {
			BigDecimal r = sqrt(BigDecimal.valueOf(i), 100);
			String s = r.toPlainString();
			System.out.println(s);
			if (r.compareTo(new BigDecimal(r.toBigInteger())) == 0) {
				continue;
			}
			for (int j = 0; j < 101; j++) {
				if (s.charAt(j) != '.') {
					sum += Character.digit(s.charAt(j), 10);
				}
			}
		}
		System.out.println(sum);
	}
}
