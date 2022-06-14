package it.pagopa.afm.marketplacebe.model.offer;

import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleOffers {

    @Schema(required = true)
    @NotNull
    @Valid
    List<BundleOffer> offers;

    @Schema(required = true)
    @NotNull
    @Valid
    PageInfo pageInfo;
}
