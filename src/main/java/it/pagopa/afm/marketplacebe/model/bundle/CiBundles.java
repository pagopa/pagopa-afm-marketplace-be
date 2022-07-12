package it.pagopa.afm.marketplacebe.model.bundle;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class CiBundles {

    @JsonProperty("bundles")
    @Schema(required = true)
    @NotNull
    @Valid
    private List<CiBundleInfo> bundleDetailsList;


    @Schema(required = true)
    @NotNull
    @Valid
    private PageInfo pageInfo;
}
