package it.pagopa.afm.marketplacebe.model.offer;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleDetails;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleCreditorInstitutionResource {

    @Schema(description = "List of details about creditor institution's subscription to a bundle", required = true)
    private List<CiBundleDetails> ciBundleDetails;

    @JsonProperty("pageInfo")
    @Schema(description = "Page info", required = true)
    @NotNull
    @Valid
    private PageInfo pageInfo;
}
