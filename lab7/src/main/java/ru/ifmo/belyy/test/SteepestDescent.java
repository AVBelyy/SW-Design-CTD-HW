package ru.ifmo.belyy.test;

import org.springframework.beans.factory.annotation.Autowired;

import java.util.function.Function;

/**
 * @author Anton Belyy <anton.belyy@gmail.com>
 * Steepest gradient descent implementation
 * for arbitrary n-dimensional functions.
 */
public class SteepestDescent {
    private double eps = 1e-6;

    @Autowired
    private GoldenSection gs;

    public Double[] compute(Function<Double[], Double> f, Function<Double[], Double[]> df, Double[] x0, Double l0, Double l1) {
        Double[] x = x0.clone();
        int n = x.length;

        while (true) {
            Double[] xPrev = x.clone();
            Function<Double, Double> func1D = (l -> {
                Double[] xD = x.clone();
                Double[] dx = df.apply(x);
                for (int i = 0; i < n; i++) {
                    xD[i] = xD[i] - l * dx[i];
                }
                return f.apply(xD);
            });
            Double lBest = gs.compute(func1D, l0, l1, null, null);
            Double l1norm = 0.;

            Double[] dx = df.apply(x);
            for (int i = 0; i < n; i++) {
                x[i] = x[i] - lBest * dx[i];
                l1norm += Math.abs(x[i] - xPrev[i]);
            }

            if (l1norm < eps) {
                break;
            }
        }

        return x;
    }
}
