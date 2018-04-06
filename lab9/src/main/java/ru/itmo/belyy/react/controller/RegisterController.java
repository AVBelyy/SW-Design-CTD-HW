package ru.itmo.belyy.react.controller;

import reactor.core.publisher.Mono;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ru.itmo.belyy.react.AlreadyExistsException;
import ru.itmo.belyy.react.model.User;
import ru.itmo.belyy.react.model.repository.UserRepository;

import javax.validation.Valid;

@RestController
public class RegisterController {
    @Autowired
    private UserRepository userRepository;

    @PostMapping("/create-user")
    public Mono<User> createUser(@Valid User user) {
        return userRepository.existsById(user.getId())
                .flatMap(alreadyExists -> {
                    if (alreadyExists) {
                        throw new AlreadyExistsException();
                    } else {
                        return userRepository.save(user);
                    }
                });
    }

    @ExceptionHandler(AlreadyExistsException.class)
    public ResponseEntity handleAlreadyExistsException(AlreadyExistsException ex) {
        return ResponseEntity.badRequest().build();
    }
}
