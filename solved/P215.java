import java.util.Arrays;

/**
 * Consider the problem of building a wall out of 2x1 and 3x1 bricks (horizontal
 * x vertical dimensions) such that, for extra strength, the gaps between
 * horizontally-adjacent bricks never line up in consecutive layers, i.e. never
 * form a "running crack".
 * <p>
 * There are eight ways of forming a crack-free 9x3 wall, written W(9,3) = 8.
 * <p>
 * Calculate W(32,10).
 */
public class P215 {

    public static int[] generateAllRows(int width) {
        int[] allRows = new int[1 << (width / 2 + 1)];
        int numberOfRows = 0;
        int row = 2;
        int k = 1; // row-in-progress end index
        boolean goingIn = true;
        while (k > 0) {
            if (goingIn) {
                // try a 2 block
                if (k + 2 > width - 1) {
                    // overshot
                    goingIn = false;
                } else if (k + 2 == width - 1) {
                    // finished the row
                    allRows[numberOfRows] = row;
                    numberOfRows++;
                    goingIn = false;
                } else {
                    // incomplete row
                    k += 2;
                    row |= (1 << k);
                }
            } else if (k == 1 || ((1 << (k - 2)) & row) != 0) {
                // switch 2 to 3
                if (k + 1 > width - 1) {
                    // overshot
                    throw new IllegalStateException();
                } else if (k + 1 == width - 1) {
                    // finished the row
                    row &= ~(1 << k);
                    allRows[numberOfRows] = row;
                    numberOfRows++;
                    k -= 2;
                } else {
                    // incomplete row
                    row &= ~(1 << k);
                    k++;
                    row |= (1 << k);
                    goingIn = true;
                }
            } else {
                // remove a 3 block
                row &= ~(1 << k);
                k -= 3;
            }
        }
        return Arrays.copyOf(allRows, numberOfRows);
    }

    public static long dynamicProgramming(int width, int height) {
        int[] allRows = generateAllRows(width);
        long[] counts = new long[allRows.length];
        Arrays.fill(counts, 1L);
        for (int h = 1; h < height; h++) {
            long[] newCounts = new long[allRows.length];
            for (int i = 0; i < newCounts.length; i++) {
                int row = allRows[i];
                long s = 0L;
                for (int j = 0; j < counts.length; j++) {
                    if ((row & allRows[j]) == 0) {
                        s += counts[j];
                    }
                }
                newCounts[i] = s;
            }
            counts = newCounts;
        }
        long total = 0;
        for (int i = 0; i < counts.length; i++) {
            total += counts[i];
        }
        return total;
    }

    public static void allInOne() {
        final int WALL_HEIGHT = 5;
        final int WALL_WIDTH = 32;
        final long WALL_END = 1L << (WALL_WIDTH - 1);
        /*
         * Let a row of bricks be represented by an integer, with set bits
         * denoting the ends of bricks, excluding the last brick on the row. The
         * row starts with low bits. For example, 0b10010 represents a 2x1
         * followed by a 3x1.
         * 
         * The rows array starts with a "foundation" at rows[0] containing no
         * cracks.
         */
        long[] rows = new long[WALL_HEIGHT + 1];
        rows[0] = 0;
        int topRow = 1;
        long rowEnd = 0;
        boolean goingIn = true;
        long numberOfWalls = 0;
        while (topRow > 0) {
            // for (int i = 1; i <= WALL_HEIGHT; i++) {
            // System.out.printf("%08x ", rows[i]);
            // }
            // System.out.printf("%d %b %08x %d\n", topRow, goingIn, rowEnd,
            // numberOfWalls);
            if (goingIn) {
                // try a 2 block
                if ((rowEnd << 2) > WALL_END) {
                    // overshot
                    goingIn = false;
                } else if ((rowEnd << 2) == WALL_END) {
                    // finished the row
                    if (topRow == WALL_HEIGHT) {
                        numberOfWalls++;
                        rowEnd = WALL_END;
                        goingIn = false;
                    } else {
                        topRow++;
                        rowEnd = 0;
                    }
                } else {
                    // incomplete row
                    if (rowEnd == 0) {
                        // first block in row
                        rowEnd = 0x2;
                        rows[topRow] = rowEnd;
                    } else {
                        rowEnd <<= 2;
                        rows[topRow] |= rowEnd;
                    }
                    if ((rows[topRow] & rows[topRow - 1]) != 0) {
                        // backtrack
                        rows[topRow] &= ~rowEnd;
                        goingIn = false;
                    }
                }
            } else if (rowEnd == 0x2 || ((rowEnd >> 2) & rows[topRow]) != 0) {
                // switch 2 to 3
                if ((rowEnd << 1) > WALL_END) {
                    // overshot
                    rowEnd >>= 2;
                } else if ((rowEnd << 1) == WALL_END) {
                    // finished the row
                    rows[topRow] &= ~rowEnd;
                    if (topRow == WALL_HEIGHT) {
                        numberOfWalls++;
                        rowEnd = WALL_END >> 3;
                    } else {
                        topRow++;
                        rowEnd = 0;
                        goingIn = true;
                    }
                } else {
                    // incomplete row
                    rows[topRow] &= ~rowEnd;
                    rowEnd <<= 1;
                    rows[topRow] |= rowEnd;
                    if ((rows[topRow] & rows[topRow - 1]) != 0) {
                        // backtrack
                        rows[topRow] &= ~rowEnd;
                        goingIn = false;
                    } else {
                        goingIn = true;
                    }
                }
            } else {
                // remove a 3 block
                rows[topRow] &= ~rowEnd;
                rowEnd >>= 3;
                if (rowEnd == 0) {
                    topRow--;
                    rowEnd = WALL_END;
                }
            }
        }
        System.out.println(numberOfWalls);
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        for (int width = 4; width <= 32; width++) {
            System.out.println(width + " " + dynamicProgramming(width, 10));
        }
    }
}
