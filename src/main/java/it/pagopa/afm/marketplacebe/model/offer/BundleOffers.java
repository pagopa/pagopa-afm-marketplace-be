package it.pagopa.afm.marketplacebe.model.offer;

import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import java.util.List;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleOffers {

  @Schema(required = true)
  @NotNull
  @Valid
  List<PspBundleOffer> offers;

  @Schema(required = true)
  @NotNull
  @Valid
  PageInfo pageInfo;
}
