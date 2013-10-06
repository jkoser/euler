/**
 * The moon has been opened up, and land can be obtained for free, but there is
 * a catch. You have to build a wall around the land that you stake out, and
 * building a wall on the moon is expensive. Every country has been allotted a
 * 500 m by 500 m square area, but they will possess only that area which they
 * wall in. 251001 posts have been placed in a rectangular grid with 1 meter
 * spacing. The wall must be a closed series of straight lines, each line
 * running from post to post.
 * 
 * The bigger countries of course have built a 2000 m wall enclosing the entire
 * 250 000 m2 area. The Duchy of Grand Fenwick, has a tighter budget, and has
 * asked you (their Royal Programmer) to compute what shape would get best
 * maximum enclosed-area/wall-length ratio.
 * 
 * Find the maximum enclosed-area/wall-length ratio. Give your answer rounded to
 * 8 places behind the decimal point in the form abc.defghijk.
 */
public class P314search {

	public static void main(String[] args) {
		for (int r = 3; r < 21; r++) {
			P314search problem = new P314search(r);
			System.out.println(problem.solve());
		}
	}

	/**
	 * Creates a new problem instance with the given radius.
	 */
	public P314search(int r) {
		this.r = r;
	}

	public double solve() {
		PathInfo best = findBestPath(new Point(0, r), new Point(r, 0), 1.0, -1.0 / 0);
		return best.area / best.perimeter;
	}

	private static class Point {
		public Point(int x, int y) {
			this.x = x;
			this.y = y;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + x;
			result = prime * result + y;
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Point other = (Point) obj;
			if (x != other.x)
				return false;
			if (y != other.y)
				return false;
			return true;
		}

		@Override
		public String toString() {
			return String.format("(%d,%d)", x, y);
		}

		public final int x;
		public final int y;
	}

	private static class PathInfo {
		public PathInfo(double area, double perimeter, double rightmostSlope) {
			this.area = area;
			this.perimeter = perimeter;
			this.rightmostSlope = rightmostSlope;
		}

		public final double area;
		public final double perimeter;
		public final double rightmostSlope;
	}

	/**
	 * Finds the path with the greatest area/perimeter ratio between two given
	 * points, p1 and p2, in the first quadrant, where {@code p1.x <= p2.x} and
	 * {@code p1.y >= p2.y}. The search is limited by the given slopes: since
	 * optimal paths are convex, this method accepts the slope of the line
	 * segment to the left of p1 and only considers paths with lesser (steeper)
	 * slope. Similarly, it accepts the slope of the line segment to the right
	 * of p2 and only considers paths with greater (shallower) slope.
	 */
	private PathInfo findBestPath(Point p1, Point p2, double slopeOnLeft, double slopeOnRight) {
		int dx = p2.x - p1.x;
		int dy = p2.y - p1.y;
		if (dx < 0 || dy > 0) {
			throw new IllegalArgumentException(String.format(
					"segment = [%s,%s], slope on left = %f", p1, p2, slopeOnLeft));
		}
		double slope = (double) dy / dx;
		if (slope >= slopeOnLeft) {
			throw new IllegalArgumentException(String.format(
					"segment = [%s,%s], slope on left = %f", p1, p2, slopeOnLeft));
		}
		// Start by using the endpoints only.
		PathInfo best = new PathInfo(
				p1.y * p2.x -
						p1.x * p1.y / 2.0 -
						p2.x * p2.y / 2.0 -
						dx * (-dy) / 2.0,
				Math.sqrt(dx * dx + dy * dy), slope);
		// If the given segment is horizontal or vertical, there are no other
		// paths to consider.
		if (p1.x == p2.x || p1.y == p2.y) {
			return best;
		}
		// Otherwise, try using intermediate points and recursing.
		for (int x3 = p1.x + 1; x3 < p2.x; x3++) {
			int yMin = (int) Math.floor(p1.y + slope * (x3 - p1.x)) + 1;
			int yMax = Math.min(
					(int) Math.ceil(p1.y + slopeOnLeft * (x3 - p1.x)) - 1,
					(int) Math.ceil(p2.y + slopeOnRight * (x3 - p2.x)) - 1);
			for (int y3 = yMin; y3 <= yMax && y3 <= p1.y; y3++) {
				Point p3 = new Point(x3, y3);
				double bSlopeMin = (p2.y - y3) / (p2.x - x3);
				PathInfo a = findBestPath(p1, p3, slopeOnLeft, bSlopeMin);
				PathInfo b = findBestPath(p3, p2, a.rightmostSlope, slopeOnRight);
				double area = a.area + b.area;
				double perimeter = a.perimeter + b.perimeter;
				if (area / perimeter > best.area / best.perimeter) {
					best = new PathInfo(area, perimeter, b.rightmostSlope);
				}
			}
		}
		// If p2 is (r, 0), try (r, y)'s.
		if (p2.x == r && p2.y == 0) {
			int yMax = (int) Math.ceil(p1.y + slopeOnLeft * (r - p1.x)) - 1;
			for (int y3 = 1; y3 <= yMax && y3 <= p1.y; y3++) {
				Point p3 = new Point(r, y3);
				PathInfo a = findBestPath(p1, p3, slopeOnLeft, -1.0 / 0);
				PathInfo b = findBestPath(p3, p2, a.rightmostSlope, slopeOnRight);
				double area = a.area + b.area;
				double perimeter = a.perimeter + b.perimeter;
				if (area / perimeter > best.area / best.perimeter) {
					best = new PathInfo(area, perimeter, b.rightmostSlope);
				}
			}
		}
		return best;
	}

	private final int r;
}
