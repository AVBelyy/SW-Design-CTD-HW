package ru.itmo.belyy.react.model.repository;

import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import ru.itmo.belyy.react.model.User;

public interface UserRepository extends ReactiveMongoRepository<User, String> {
}
