package it.pagopa.afm.marketplacebe.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;

import javax.validation.constraints.NotNull;


@Getter
public enum AppError {
    INTERNAL_SERVER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "Internal Server Error", "Something was wrong"),
    ENTITY_VALIDATION_FAIL(HttpStatus.INTERNAL_SERVER_ERROR, "Error during entity validation", "%s"),

    BUNDLE_BAD_REQUEST(HttpStatus.BAD_REQUEST, "Bundle bad request", "Bundle data not valid. %s"),
    BUNDLE_NOT_FOUND(HttpStatus.NOT_FOUND, "Bundle not found", "Bundle with id %s not found."),
    BUNDLE_PSP_CONFLICT(HttpStatus.CONFLICT, "Bundle conflict", "Bundle with id %s and idPsp: %s"),

    BUNDLE_NAME_CONFLICT(HttpStatus.CONFLICT, "Bundle conflict", "Bundle with name %s"),

    BUNDLE_OFFER_BAD_REQUEST(HttpStatus.BAD_REQUEST, "Bundle offer bad request", "Bundle offer with id %s not configured with %s."),
    BUNDLE_OFFER_BAD_ATTRIBUTE(HttpStatus.BAD_REQUEST, "Bundle offer bad attribute", "Bundle offer with id %s has invalid attributes: %s."),
    BUNDLE_OFFER_NOT_FOUND(HttpStatus.NOT_FOUND, "Bundle offer not found", "Bundle offer with id %s not found."),
    BUNDLE_OFFER_CONFLICT(HttpStatus.CONFLICT, "Bundle offer conflict", "Bundle offer with id %s. %s"),
    BUNDLE_OFFER_ALREADY_ACCEPTED(HttpStatus.CONFLICT, "Bundle Offer already accepted", "The offer %s was accepted on %s"),
    BUNDLE_OFFER_ALREADY_REJECTED(HttpStatus.CONFLICT, "Bundle Offer already rejected", "The offer %s was rejected on %s"),

    TOUCHPOINT_NOT_FOUND(HttpStatus.NOT_FOUND, "Touchpoint not found", "Touchpoint %s not found"),
    TOUCHPOINT_CONFLICT(HttpStatus.CONFLICT, "Touchpoint conflict", "Touchpoint with name %s already exists."),
    TOUCHPOINT_BAD_REQUEST(HttpStatus.BAD_REQUEST, "Touchpoint bad request", "Touchpoint with id %s."),

    PAYMENT_TYPE_NOT_FOUND(HttpStatus.NOT_FOUND, "Payment type not found", "Payment type %s not found"),
    PAYMENT_TYPE_NOT_DELETABLE(HttpStatus.BAD_REQUEST, "Payment type associated to bundle", "Payment type %s is associated to an existent bundle."),
    PAYMENT_TYPE_CONFLICT(HttpStatus.CONFLICT, "Payment type conflict", "Payment type with name %s already exists"),

    CI_BUNDLE_BAD_REQUEST(HttpStatus.BAD_REQUEST, "CI-BUNDLE bad request", "Problem to create CI-BUNDLE. %s"),
    CI_BUNDLE_NOT_FOUND(HttpStatus.NOT_FOUND, "No CI-BUNDLE relationship found", "Relation between Bundle %s and CI %s not found."),
    CI_BUNDLE_ID_NOT_FOUND(HttpStatus.NOT_FOUND, "No CI-BUNDLE relationship found", "CI-Bundle %s not found."),

    BUNDLE_REQUEST_BAD_REQUEST(HttpStatus.BAD_REQUEST, "Bundle request bad request", "Bundle request with id %s not configured with %s."),
    BUNDLE_REQUEST_BAD_ATTRIBUTE(HttpStatus.BAD_REQUEST, "Bundle request bad attribute", "Bundle request with id %s has invalid attributes: %s."),
    BUNDLE_REQUEST_BAD_DATE(HttpStatus.BAD_REQUEST, "Bundle request bad request", "Date %s is not valid."),
    BUNDLE_REQUEST_NOT_FOUND(HttpStatus.NOT_FOUND, "Bundle request not found", "Bundle request with id %s not found."),
    BUNDLE_REQUEST_CONFLICT(HttpStatus.CONFLICT, "Bundle request conflict", "Bundle request with id %s. %s"),

    REQUEST_ALREADY_ACCEPTED(HttpStatus.CONFLICT, "Request already accepted", "The request %s was accepted on %s"),
    REQUEST_ALREADY_REJECTED(HttpStatus.CONFLICT, "Request already rejected", "The request %s was rejected on %s"),

    BUNDLE_ATTRIBUTE_NOT_FOUND(HttpStatus.NOT_FOUND, "Bundle attribute not found", "Bundle attribute with id %s not found."),

    BUNDLE_ATTRIBUTE_NOT_INITIALIZED(HttpStatus.CONFLICT, "Bundle attribute element is null", "CiBundle with id %s has attribute field null. Contact tech support"),

    CALCULATOR_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "Internal Server Error", "Something was wrong generating configuration"),

    PAYMENT_METHOD_NOT_FOUND(
            HttpStatus.NOT_FOUND,
            "Payment method not found",
            "Payment method with id %s not found"),
    PAYMENT_METHOD_MULTIPLE_FOUND(
            HttpStatus.INTERNAL_SERVER_ERROR,
            "Payment method multiple found",
            "Payment method with id %s multiple found, contact technical support");


    public final HttpStatus httpStatus;
    public final String title;
    public final String details;


    AppError(HttpStatus httpStatus, String title, String details) {
        this.httpStatus = httpStatus;
        this.title = title;
        this.details = details;
    }
}


