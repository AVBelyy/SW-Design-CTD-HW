package ru.ifmo.belyy.aspect;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;

import java.util.*;

/**
 * @author Anton Belyy <anton.belyy@gmail.com>
 */
@Aspect
public class ProfileAspect {
    public static class MethodStats {
        private int numEvals;
        private long time;

        MethodStats(int numEvals, long time) {
            this.numEvals = numEvals;
            this.time = time;
        }

        int getNumEvals() {
            return numEvals;
        }

        long getAllTime() {
            return time;
        }

        double getAvgTime() {
            return time * 1.0 / numEvals;
        }
    }

    private String packageName = "ru.ifmo.belyy.test"; // Constant for simplicity
    private String[] springMethods = {"$$EnhancerBySpringCGLIB$$", "$$FastClassBySpringCGLIB$$"};
    private Map<String, MethodStats> statsMap = new TreeMap<>();

    @Around("execution(* ru.ifmo.belyy.test.*.*(..))")
    public Object logExecutionTime(ProceedingJoinPoint joinPoint) throws Throwable {
        long startTime = System.nanoTime();

        // tokens :: [modifier, return type, method signature]
        String[] tokens = joinPoint.getSignature().toLongString().split(" ");
        String method = tokens[2];

        // Parse stack trace
        StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
        StringBuilder sb = new StringBuilder();
        for (StackTraceElement stElem : stackTrace) {
            String className = stElem.getClassName();
            // Remove Spring wrappers from trace
            boolean springFlag = false;
            for (String springMethod : springMethods) {
                if (className.contains(springMethod)) {
                    springFlag = true;
                    break;
                }
            }
            if (!springFlag && className.startsWith(this.packageName)) {
                String methodName = stElem.getMethodName();
                sb.append(className).append('.').append(methodName).append('/');
            }
        }
        String stack = sb.append(method).toString();
        //System.out.println("->" +  stack + "<-");

        // Execute wrapped action
        Object result = joinPoint.proceed(joinPoint.getArgs());

        long totalTime = System.nanoTime() - startTime;

        // Update stats map
        MethodStats methodStats = this.statsMap.get(stack);
        if (methodStats == null) {
            this.statsMap.put(stack, new MethodStats(1, totalTime));
        } else {
            methodStats.numEvals++;
            methodStats.time += totalTime;
            this.statsMap.put(stack, methodStats);
        }
        return result;
    }

    public void printStats(int tabWidth) {
        String[] path = new String[0];
        for (Map.Entry<String, MethodStats> entry: statsMap.entrySet()) {
            int l = 0;
            String[] cur = entry.getKey().split("/");

            while (l < path.length && path[l].startsWith(cur[l])) {
                l++;
            }
            StringBuilder sb = new StringBuilder();
            for (int j = 0; j < l * tabWidth; j++) {
                sb.append(' ');
            }

            while (l < cur.length) {
                System.out.print(sb.toString());
                if (l + 1 != cur.length) {
                    System.out.println(cur[l]);
                }
                else {
                    System.out.format("%s: all time = %.3f ms, avg time = %.3f ms, num evals = %d\n",
                            cur[l],
                            entry.getValue().getAllTime() / 1e6,
                            entry.getValue().getAvgTime() / 1e6,
                            entry.getValue().getNumEvals());
                }
                for (int j = 0; j < tabWidth; j++) {
                    sb.append(' ');
                }
                l++;
            }
            path = cur;
        }
    }
}
