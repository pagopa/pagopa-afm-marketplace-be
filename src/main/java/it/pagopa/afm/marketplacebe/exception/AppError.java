package it.pagopa.afm.marketplacebe.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;


@Getter
public enum AppError {
    INTERNAL_SERVER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "Internal Server Error", "Something was wrong"),
    ENTITY_VALIDATION_FAIL(HttpStatus.INTERNAL_SERVER_ERROR, "Error during entity validation", "%s"),
    BUNDLE_NOT_FOUND(HttpStatus.NOT_FOUND, "Bundle not found", "Bundle with id %s not found."),
    CI_BUNDLE_NOT_FOUND(HttpStatus.NOT_FOUND, "No relation CI-BUNDLE found", "Relation between Bundle %s and CI %s not found."),
    BUNDLE_OFFER_BAD_REQUEST(HttpStatus.BAD_REQUEST, "Bundle offer bad request", "Bundle offer with id %s not configured with %s."),
    BUNDLE_OFFER_NOT_FOUND(HttpStatus.NOT_FOUND, "Bundle offer not found", "Bundle offer with id %s not found."),
    BUNDLE_REQUEST_NOT_FOUND(HttpStatus.NOT_FOUND, "Bundle request not found", "Bundle request with id %s not found."),
    REQUEST_ALREADY_ACCEPTED(HttpStatus.CONFLICT, "Request already accepted", "The request {} was accepted on {}"),
    REQUEST_ALREADY_REJECTED(HttpStatus.CONFLICT, "Request already rejected", "The request {} was rejected on {}"),
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


