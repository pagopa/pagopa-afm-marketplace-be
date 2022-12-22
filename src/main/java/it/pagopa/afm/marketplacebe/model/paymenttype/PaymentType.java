package it.pagopa.afm.marketplacebe.model.paymenttype;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class PaymentType {

    @JsonProperty("paymentType")
    @Schema(required = true)
    @NotNull
    @Valid
    private String name;


    @Schema(required = true)
    @NotNull
    @Valid
    private Boolean used;
}
