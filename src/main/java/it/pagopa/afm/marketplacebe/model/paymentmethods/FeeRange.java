package it.pagopa.afm.marketplacebe.model.paymentmethods;

import javax.validation.constraints.NotNull;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class FeeRange {
  @NotNull Long min;

  @NotNull Long max;
}
