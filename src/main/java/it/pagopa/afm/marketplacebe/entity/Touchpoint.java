package it.pagopa.afm.marketplacebe.entity;

import it.pagopa.afm.marketplacebe.exception.AppException;
import lombok.Getter;
import org.springframework.http.HttpStatus;

import java.util.Arrays;

@Getter
public enum Touchpoint {
    IO("IO"),
    WISP("WISP"),
    CHECKOUT("CHECKOUT");

    private final String value;

    Touchpoint (final String touchpoint) {
        this.value = touchpoint;
    }

    public static Touchpoint fromValue(String value) {
        return Arrays.stream(Touchpoint.values())
                .filter(elem -> elem.value.equals(value))
                .findFirst()
                .orElseThrow(() -> new AppException(HttpStatus.INTERNAL_SERVER_ERROR, "Touchpoint not found", "Cannot convert string '" + value + "' into enum"));
    }
}
