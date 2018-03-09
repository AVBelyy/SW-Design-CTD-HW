package ru.ifmo.belyy.test;

import java.util.function.Function;

/**
 * @author Anton Belyy <anton.belyy@gmail.com>
 */
public class GoldenSection {
    private double eps = 1e-6;
    private double goldenRatio = 1.61803398875;

    public Double compute(Function<Double, Double> f, Double a, Double b, Double y1, Double y2) {
        if (Math.abs(b - a) < eps) {
            return (a + b) / 2;
        } else {
            Double x1 = b - (b - a) / goldenRatio;
            Double x2 = a + (b - a) / goldenRatio;
            if (y1 == null) {
                y1 = f.apply(x1);
            }
            if (y2 == null) {
                y2 = f.apply(x2);
            }
            if (y1 >= y2) {
                return compute(f, x1, b, y2, null);
            } else {
                return compute(f, a, x2, null, y1);
            }
        }
    }
}
