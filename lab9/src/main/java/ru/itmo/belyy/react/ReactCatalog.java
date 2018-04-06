package ru.itmo.belyy.react;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.mongodb.repository.config.EnableReactiveMongoRepositories;

@SpringBootApplication
@EnableReactiveMongoRepositories
public class ReactCatalog {
    public static void main(String[] args)  {
        SpringApplication.run(ReactCatalog.class, args);
    }
}