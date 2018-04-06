package ru.itmo.belyy.react.model.repository;

import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import ru.itmo.belyy.react.model.Product;

public interface ProductRepository extends ReactiveMongoRepository<Product, String> {
}
