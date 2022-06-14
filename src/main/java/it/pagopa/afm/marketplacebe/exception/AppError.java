package it.pagopa.afm.marketplacebe.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;


@Getter
public enum AppError {
    INTERNAL_SERVER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "Internal Server Error", "Something was wrong"),
    ENTITY_VALIDATION_FAIL(HttpStatus.INTERNAL_SERVER_ERROR, "Error during entity validation", "%s"),

    BUNDLE_OFFER_BAD_REQUEST(HttpStatus.BAD_REQUEST, "Bundle offer bad request", "Bundle offer with id %s not configured with %s."),
    BUNDLE_OFFER_NOT_FOUND(HttpStatus.NOT_FOUND, "Bundle offer not found", "Bundle offer with id %s not found."),

    UNKNOWN(null, null, null);

    public final HttpStatus httpStatus;
    public final String title;
    public final String details;


    AppError(HttpStatus httpStatus, String title, String details) {
        this.httpStatus = httpStatus;
        this.title = title;
        this.details = details;
    }
}


