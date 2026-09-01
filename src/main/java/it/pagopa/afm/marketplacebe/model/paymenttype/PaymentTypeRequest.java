package it.pagopa.afm.marketplacebe.model.paymenttype;

import io.swagger.v3.oas.annotations.media.Schema;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import lombok.*;

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
