public class P4 {
	public static boolean isPalindrome(String s) {
		int length = s.length();
		for (int i = 0; i < length / 2; i++) {
			if (s.charAt(i) != s.charAt(length - i - 1))
				return false;
		}
		return true;
	}

	public static void main(String[] args) {
		int greatest = 0;
		for (int j = 100; j < 1000; j++) {
			for (int k = j; k < 1000; k++) {
				int n = j * k;
				if (isPalindrome(String.valueOf(n)))
					greatest = Math.max(n,greatest);
			}
		}
		System.out.println(greatest);
	}
}
