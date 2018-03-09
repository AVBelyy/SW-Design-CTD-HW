package ru.ifmo.belyy;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import ru.ifmo.belyy.aspect.*;
import ru.ifmo.belyy.test.GoldenSection;
import ru.ifmo.belyy.test.SteepestDescent;

/**
 * @author akirakozov
 * @author belyy
 */
@Configuration
@EnableAspectJAutoProxy
public class ContextConfiguration {
    @Bean
    public GoldenSection gsBean() {
        return new GoldenSection();
    }

    @Bean
    public SteepestDescent sdBean() {
        return new SteepestDescent();
    }

    @Bean
    public ProfileAspect profileAspect() {
        return new ProfileAspect();
    }
}
