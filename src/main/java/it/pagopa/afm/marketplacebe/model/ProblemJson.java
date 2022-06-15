package it.pagopa.afm.marketplacebe.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

/**
 * Object returned as response in case of an error.
 * <p> See {@link it.pagopa.afm.marketplacebe.exception.ErrorHandler}
 */
@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProblemJson {

    @JsonProperty("title")
    @Schema(description = "A short, summary of the problem type. Written in english and readable for engineers (usually not suited for non technical stakeholders and not localized); example: Service Unavailable")
    private String title;

    @JsonProperty("status")
    @Schema(example = "200", description = "The HTTP status code generated by the origin server for this occurrence of the problem.")
    @Min(100)
    @Max(600)
    private Integer status;

    @JsonProperty("detail")
    @Schema(example = "There was an error processing the request", description = "A human readable explanation specific to this occurrence of the problem.")
    private String detail;

}
