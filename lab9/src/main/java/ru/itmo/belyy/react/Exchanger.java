package ru.itmo.belyy.react;

import ru.itmo.belyy.react.model.Product;

import java.util.HashMap;
import java.util.Map;

public class Exchanger {
    private final static Map<String, Double> CURRENCIES = new HashMap<>();

    // Should be also updated reactively
    static {
        CURRENCIES.put("usd", 58.14);
        CURRENCIES.put("eur", 71.42);
        CURRENCIES.put("rub", 1.0);
    }

    public static Product convertFromRub(Product product, String currency) {
        final Double coef = CURRENCIES.getOrDefault(currency, 1.0);
        final Double prevPrice = product.getPrice();

        product.setPrice(prevPrice / coef);
        return product;
    }

    public static Product convertToRub(Product product, String currency) {
        final Double coef = CURRENCIES.getOrDefault(currency, 1.0);
        final Double prevPrice = product.getPrice();

        product.setPrice(prevPrice * coef);
        return product;
    }
}
