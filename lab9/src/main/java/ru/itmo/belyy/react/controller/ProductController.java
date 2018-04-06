package ru.itmo.belyy.react.controller;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.beans.factory.annotation.Autowired;
import ru.itmo.belyy.react.Exchanger;
import ru.itmo.belyy.react.AlreadyExistsException;
import ru.itmo.belyy.react.model.Product;
import ru.itmo.belyy.react.model.repository.UserRepository;
import ru.itmo.belyy.react.model.repository.ProductRepository;

import javax.validation.Valid;

@RestController
public class ProductController {
    @Autowired
    private ProductRepository productRepository;
    @Autowired
    private UserRepository userRepository;

    @GetMapping("/get-products")
    public Flux<Product> getAllProducts(@RequestParam String id) {
        Flux<Product> products = productRepository.findAll();
        return userRepository.findById(id)
                .flatMapMany(user -> products
                        .map(product -> convertFromRub(user.getCurrency(), product)));
    }

    @PostMapping("/add-product")
    public Mono<Product> addProduct(@Valid Product product, @RequestParam String id) {
        return productRepository
                .existsById(product.getName())
                .flatMap(alreadyExists -> {
                    if (alreadyExists) {
                        throw new AlreadyExistsException();
                    }
                    return productRepository
                            .saveAll(userRepository.findById(id)
                            .map(user -> convertToRub(user.getCurrency(), product)))
                            .next();
                });
    }

    @ExceptionHandler(AlreadyExistsException.class)
    public ResponseEntity handleAlreadyExistsException(AlreadyExistsException e) {
        return ResponseEntity.badRequest().build();
    }

    private Product convertToRub(String currency, Product product) {
        return Exchanger.convertToRub(product, currency);
    }

    private Product convertFromRub(String currency, Product product) {
        return Exchanger.convertFromRub(product, currency);
    }
}
