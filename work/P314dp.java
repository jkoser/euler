import java.util.ArrayList;
import java.util.List;

import org.javatuples.Pair;

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
public class P314dp {

	public static void sliceAndDice(int r) {
		PointInfo[][] points = new PointInfo[r + 1][];
		PointInfo p;

		// top down, and left to right
		for (int y = r; y >= 0; y--) {
			points[y] = new PointInfo[r + 1];
			p = new PointInfo();
			p.terminalLeft = true;
			p.areaLeft = 0.0;
			p.perimLeft = 0.0;
			points[y][y] = p;
			if (y < r) {
				double x0 = y + 0.5, y0 = y + 0.5;
				p = new PointInfo();
				p.terminalLeft = true;
				// enclosing rectangle minus three corners
				p.areaLeft = (y + 1) * y0 -
						0.125 - y0 * x0 / 2 - (y + 1) * y / 2;
				p.perimLeft = Math.sqrt(2) / 2;
				points[y + 1][y] = p;
			}
			for (int x = y + 2; x <= r; x++) {
				double bestRatio = 0.0, bestArea = 0.0, bestPerim = 0.0;
				int bestX0 = 0, bestY0 = 0;
				for (int dy = x - y; dy > 0; dy--) {
					for (int dx = -Math.min(dy, x - y - dy); dx <= 0; dx++) {
						int x0 = x + dx, y0 = y + dy;
						PointInfo p0 = points[x0][y0];
						// enclosing rectangle minus three corners
						double a = p0.areaLeft + x * y0 -
								(-dx) * dy / 2.0 - y0 * x0 / 2.0 - x * y / 2.0;
						double m = p0.perimLeft + Math.sqrt(dx * dx + dy * dy);
						double ratio = a / m;
						if (ratio > bestRatio) {
							bestRatio = ratio;
							bestArea = a;
							bestPerim = m;
							bestX0 = x0;
							bestY0 = y0;
						}
					}
				}
				p = new PointInfo();
				p.areaLeft = bestArea;
				p.perimLeft = bestPerim;
				p.xLeft = bestX0;
				p.yLeft = bestY0;
				points[x][y] = p;
			}
		}

		// bottom up, and right to left
		for (int x = 0; x <= r; x++) {
			p = points[x][0];
			p.terminalDown = true;
			p.areaDown = 0.0;
			p.perimDown = 0.0;
		}
		for (int y = 1; y <= r; y++) {
			for (int x = r; x >= y; x--) {
				double bestRatio = 0.0, bestArea = 0.0, bestPerim = 0.0;
				int bestX0 = 0, bestY0 = 0;
				for (int dy = -y; dy < 0; dy++) {
					for (int dx = r - x; dx >= 0; dx--) {
						int x0 = x + dx, y0 = y + dy;
						PointInfo p0 = points[x0][y0];
						// enclosing rectangle minus three corners
						double a = p0.areaDown + x0 * y -
								dx * (-dy) / 2.0 - x0 * y0 / 2.0 - x * y / 2.0;
						double m = p0.perimDown + Math.sqrt(dx * dx + dy * dy);
						double ratio = a / m;
						if (ratio > bestRatio) {
							bestRatio = ratio;
							bestArea = a;
							bestPerim = m;
							bestX0 = x0;
							bestY0 = y0;
						}
					}
				}
				p = points[x][y];
				p.areaDown = bestArea;
				p.perimDown = bestPerim;
				p.xDown = bestX0;
				p.yDown = bestY0;
			}
		}

		// find the best overall ratio
		double bestRatio = 0.0;
		String bestArc = null;
		String bestSlopes = null;
		for (int x = 1; x <= r; x++) {
			for (int y = 0; y <= x; y++) {
				p = points[x][y];
				double ratio = (p.areaDown + p.areaLeft) / (p.perimDown + p.perimLeft);
				if (ratio > bestRatio) {
					bestRatio = ratio;
					List<Pair<Integer, Integer>> arc = new ArrayList<>();
					arc.add(new Pair<>(x, y));
					List<Double> slopes = new ArrayList<>();
					int x1 = x, y1 = y;
					PointInfo p1 = points[x1][y1];
					while (!p1.terminalLeft) {
						slopes.add(0, (double) (y1 - p1.yLeft) / (x1 - p1.xLeft));
						x1 = p1.xLeft;
						y1 = p1.yLeft;
						p1 = points[x1][y1];
						arc.add(0, new Pair<>(x1, y1));
					}
					p1 = points[x1][y1];
					while (!p1.terminalDown) {
						slopes.add((double) (p1.yDown - y1) / (p1.xDown - x1));
						x1 = p1.xDown;
						y1 = p1.yDown;
						p1 = points[x1][y1];
						arc.add(new Pair<>(x1, y1));
					}
					bestArc = arc.toString();
					bestSlopes = slopes.toString();
				}
			}
		}
		System.out.println(bestArc);
		System.out.println(bestSlopes);
		System.out.println(bestRatio);
	}

	public static void main(String[] args) {
		Utils.timeThis(new Runnable() {
			@Override
			public void run() {
				for (int r = 3; r < 50; r++) {
					sliceAndDice(r);
				}
			}
		});
	}

	private static class PointInfo {
		// area and perimeter along the best path toward y-axis
		double areaLeft, perimLeft;
		// coordinates of the next point in the best path toward y-axis
		int xLeft, yLeft;
		// true if there is no next point to the left
		boolean terminalLeft;
		// area and perimeter along the best path toward x-axis
		double areaDown, perimDown;
		// coordinates of the next point in the best path toward x-axis
		int xDown, yDown;
		// true if there is no next point down
		boolean terminalDown;
	}
}
