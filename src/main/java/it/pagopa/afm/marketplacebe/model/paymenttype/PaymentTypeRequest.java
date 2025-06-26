package it.pagopa.afm.marketplacebe.model.paymenttype;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class PaymentTypeRequest {

    @Schema(required = true)
    @NotNull
    @Valid
    private String name;


    @Schema(required = true)
    @NotNull
    @Valid
    private String description;
}
