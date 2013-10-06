import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.uncommons.maths.random.MersenneTwisterRNG;
import org.uncommons.watchmaker.framework.EvolutionObserver;
import org.uncommons.watchmaker.framework.EvolutionaryOperator;
import org.uncommons.watchmaker.framework.FitnessEvaluator;
import org.uncommons.watchmaker.framework.PopulationData;
import org.uncommons.watchmaker.framework.SelectionStrategy;
import org.uncommons.watchmaker.framework.factories.AbstractCandidateFactory;
import org.uncommons.watchmaker.framework.islands.IslandEvolution;
import org.uncommons.watchmaker.framework.islands.IslandEvolutionObserver;
import org.uncommons.watchmaker.framework.islands.RingMigration;
import org.uncommons.watchmaker.framework.operators.AbstractCrossover;
import org.uncommons.watchmaker.framework.operators.EvolutionPipeline;
import org.uncommons.watchmaker.framework.selection.RouletteWheelSelection;
import org.uncommons.watchmaker.framework.termination.ElapsedTime;

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
public class P314ga {

    public static final int R = 250;

    /**
     * GA candidate arcs are lists of points (x, y) where 0 < y <= x <= R. The
     * points specify the posts found along one-eighth of the wall, where the
     * wall is assumed to be symmetric about the vertical, horizontal, and
     * diagonal axes. Points are ordered by descending y, and ys are distinct.
     * Points are also ordered by ascending x, and xs are distinct. There is an
     * implied point at (R, 0), which is an exception to the distinctness.
     */
    public static final class Point {
        public Point(int x, int y) {
            if (x > R || y > x || y <= 0) {
                throw new IllegalArgumentException();
            }
            this.x = x;
            this.y = y;
        }

        @Override
        public String toString() {
            return String.format("(%d, %d)", x, y);
        }

        public final int x;
        public final int y;
    }

    public static class ArcFactory extends
            AbstractCandidateFactory<List<Point>> {

        @Override
        public List<Point> generateRandomCandidate(Random rng) {
            List<Point> arc = new ArrayList<Point>();
            int y = R - rng.nextInt(R / 2);
            int x = y + rng.nextInt(R - y + 1);
            arc.add(new Point(x, y));
            while (x < R && y > 1) {
                y = y - 1 - rng.nextInt(y / 2);
                x = x + 1 + rng.nextInt(R - x);
                arc.add(new Point(x, y));
            }
            // System.out.printf("generate: %s%n", arc);
            return arc;
        }
    }

    public static class ArcCrossover extends AbstractCrossover<List<Point>> {

        public ArcCrossover() {
            super(1);
        }

        @Override
        protected List<List<Point>> mate(List<Point> parent1,
                List<Point> parent2, int numberOfCrossoverPoints, Random rng)
        {
            if (numberOfCrossoverPoints != 1) {
                throw new IllegalArgumentException();
            }
            int yCross = 1 + rng.nextInt(R - 1);
            // System.out.printf("mate:%n%s%n%s%n yCross: %d%n", parent1,
            // parent2,
            // yCross);
            int xCross = 0;
            List<Point> child1 = new ArrayList<Point>();
            for (Point p : parent1) {
                if (p.y > yCross) {
                    child1.add(p);
                    xCross = p.x;
                } else {
                    break;
                }
            }
            for (Point p : parent2) {
                if (p.y <= yCross && p.x > xCross) child1.add(p);
            }
            if (child1.isEmpty()) child1 = parent1;
            List<Point> child2 = new ArrayList<Point>();
            for (Point p : parent2) {
                if (p.y > yCross) {
                    child2.add(p);
                    xCross = p.x;
                } else {
                    break;
                }
            }
            for (Point p : parent1) {
                if (p.y <= yCross && p.x > xCross) child2.add(p);
            }
            if (child2.isEmpty()) child2 = parent2;
            List<List<Point>> pair = Arrays.asList(child1, child2);
            return pair;
        }
    }

    public static class ArcMutation implements
            EvolutionaryOperator<List<Point>> {

        @Override
        public List<List<Point>> apply(List<List<Point>> selectedCandidates,
                Random rng)
        {
            List<List<Point>> mutants = new ArrayList<List<Point>>(
                selectedCandidates.size());
            candidates: for (List<Point> arc : selectedCandidates) {
                int n = arc.size();
                List<Point> mutant = new ArrayList<Point>(arc);
                for (int k = 0; k <= rng.nextInt(5); k++) {
                    int i = rng.nextInt(n);
                    int x = mutant.get(i).x;
                    int y = mutant.get(i).y;
                    boolean horizontal = rng.nextBoolean();
                    // System.out.printf(
                    // "mutate:%n%s%nk=%d, i=%d: (%d, %d), horizontal? %s%n",
                    // mutant, k, i, x, y, horizontal);
                    if (horizontal) {
                        int xMin = y;
                        if (i > 0) xMin = mutant.get(i - 1).x + 1;
                        int xMax = R;
                        if (i < n - 1) xMax = mutant.get(i + 1).x - 1;
                        if (xMin > xMax) {
                            mutants.add(mutant);
                            continue candidates;
                        } else {
                            x = xMin + rng.nextInt(xMax - xMin + 1);
                        }
                    } else {
                        int yMin = 1;
                        if (i < n - 1) yMin = mutant.get(i + 1).y + 1;
                        yMin = Math.max(yMin, y - 1);
                        int yMax = x;
                        if (i > 0) yMax = mutant.get(i - 1).y - 1;
                        yMax = Math.min(yMax, y + 1);
                        y = yMin + rng.nextInt(yMax - yMin + 1);
                    }
                    // System.out.printf("-> (%d, %d)%n", x, y);
                    mutant.set(i, new Point(x, y));
                }
                mutants.add(mutant);
            }
            return mutants;
        }

    }

    public static class ArcEvaluator implements FitnessEvaluator<List<Point>> {

        @Override
        public double getFitness(List<Point> candidate,
                List<? extends List<Point>> population)
        {
            int n = candidate.size();
            if (n == 0) return 0;
            double x0 = candidate.get(0).x;
            double y0 = candidate.get(0).y;
            if (x0 < y0 || y0 > R) return 0;
            double a = (x0 * x0 - (x0 - y0) * (x0 - y0) / 2 - x0 * y0) / 2;
            double p = Math.sqrt(2) * (x0 - y0) / 2;
            for (int i = 0; i < n - 1; i++) {
                x0 = candidate.get(i).x;
                y0 = candidate.get(i).y;
                double x1 = candidate.get(i + 1).x;
                double y1 = candidate.get(i + 1).y;
                if (x0 >= x1 || y1 >= y0) return 0;
                a += y0 * x1 - x0 * y0 / 2 - (y0 - y1) * (x1 - x0) / 2 - x1 *
                    y1 / 2;
                p += Math.sqrt((y0 - y1) * (y0 - y1) + (x1 - x0) * (x1 - x0));
            }
            x0 = candidate.get(n - 1).x;
            y0 = candidate.get(n - 1).y;
            if (x0 > R) return 0;
            a += y0 * R / 2;
            p += Math.sqrt(y0 * y0 + (R - x0) * (R - x0));
            return Math.max(Math.pow(Math.exp(a / p - 132.5), 5), 0);
            // return Math.max(Math.exp(a / p - 132.5) - 0.75, 0);
            // return Math.max(a / p - 125, 0);
        }

        @Override
        public boolean isNatural() {
            return true;
        }
    }

    public static class Observer implements EvolutionObserver<List<Point>> {

        @Override
        public void populationUpdate(PopulationData<? extends List<Point>> data)
        {
            System.out.printf("%d: %.6f %s%n",
                data.getGenerationNumber(),
                data.getBestCandidateFitness(),
                data.getBestCandidate());
        }
    }

    public static void main(String[] args) {
        ArcFactory arcFactory = new ArcFactory();
        List<EvolutionaryOperator<List<Point>>> operators = Arrays.asList(
            new ArcCrossover(), new ArcMutation());
        EvolutionaryOperator<List<Point>> pipelineOperator =
            new EvolutionPipeline<List<Point>>(
                operators);
        ArcEvaluator arcEvaluator = new ArcEvaluator();
        SelectionStrategy<Object> selection = new RouletteWheelSelection();
        Random rng = new MersenneTwisterRNG();
        // EvolutionEngine<List<Point>> engine =
        // new GenerationalEvolutionEngine<List<Point>>(
        // arcFactory, pipelineOperator,
        // arcEvaluator,
        // selection, rng);
        // engine.addEvolutionObserver(new Observer());
        // List<Point> winner = engine.evolve(5000, 100, new ElapsedTime(5000));
        IslandEvolution<List<Point>> engine =
            new IslandEvolution<List<Point>>(5, // Number of islands.
                new RingMigration(),
                arcFactory,
                pipelineOperator,
                arcEvaluator,
                selection,
                rng);
        engine.addEvolutionObserver(new IslandEvolutionObserver<List<Point>>() {

            @Override
            public void populationUpdate(
                    PopulationData<? extends List<Point>> data)
            {
            }

            @Override
            public void islandPopulationUpdate(int islandIndex,
                    PopulationData<? extends List<Point>> data)
            {
                System.out.printf("(%d) %d: %.6f %s%n",
                    islandIndex,
                    data.getGenerationNumber(),
                    data.getBestCandidateFitness(),
                    data.getBestCandidate());

            }
        });
        engine.evolve(1000, // Population size per island.
            10, // Elitism for each island.
            20, // Epoch length (no. generations).
            5, // Migrations from each island at each epoch.
            new ElapsedTime(60000));
    }
}
