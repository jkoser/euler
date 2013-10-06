import java.util.*;

public class PrimeSieve {
    private Set<Integer> primeSet;
    private List<Integer> primeList;

    public PrimeSieve(int limit) {
        primeList = new ArrayList<Integer>();
        primeList.add(2);
        byte[] composite = new byte[limit];
        for (int i = 3; i < limit; i += 2) {
            if (composite[i] == 0) {
                primeList.add(i);
                int j = i * 3;
                int s = i * 2;
                while (j < limit) {
                    composite[j] = (byte) 1;
                    j += s;
                }
            }
        }
        primeSet = new HashSet<Integer>(primeList);
    }

    public List<Integer> getList() {
        return primeList;
    }

    public boolean test(int n) {
        return primeSet.contains(n);
    }

    public static void main(String[] args) {
        int limit = Integer.parseInt(args[0]);
        PrimeSieve sieve = new PrimeSieve(limit);
        System.out.println(sieve.getList().size());
    }
}
