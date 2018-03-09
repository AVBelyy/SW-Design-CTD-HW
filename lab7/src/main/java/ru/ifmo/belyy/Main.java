package ru.ifmo.belyy;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import ru.ifmo.belyy.aspect.ProfileAspect;
import ru.ifmo.belyy.test.SteepestDescent;


public class Main {
    private static Double f(Double[] p) {
        double x = p[0], y = p[1];
        double a = 3.0, b = 1.0, c = 0.2, d = 0.0;
        return a * Math.exp(Math.pow(x - y + c, 2)) + b * Math.exp(Math.pow(x + y + d, 2));
    }

    private static Double[] df(Double[] p) {
        double x = p[0], y = p[1];
        double a = 3.0, b = 1.0, c = 0.2, d = 0.0;
        double df0 = 2 * a * Math.exp(Math.pow(c + x - y, 2)) * (c + x - y);
        double df1 = 2 * b * Math.exp(Math.pow(d + x + y, 2)) * (d + x + y);
        double dfx = +df0 + df1;
        double dfy = -df0 + df1;
        return new Double[]{dfx, dfy};
    }

    public static void main(String[] args) {
        ApplicationContext ctx =
                new AnnotationConfigApplicationContext(ContextConfiguration.class);

        Double[] x0 = {-1.0, 0.0};
        SteepestDescent steepestDescent = ctx.getBean(SteepestDescent.class);
        Double[] x = steepestDescent.compute(Main::f, Main::df, x0, 0., 1.);

        // Should be: (-0.100, 0.100), but this assignment is not about optimization theory...
        System.out.format("(x_min, y_min) for f = (%.3f, %.3f)\n", x[0], x[1]);

        ProfileAspect profiler = ctx.getBean(ProfileAspect.class);
        profiler.printStats(2);
    }
}
