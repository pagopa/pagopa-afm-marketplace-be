package it.pagopa.afm.marketplacebe.model.touchpoint;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.PspBundleDetails;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class Touchpoints {

    @JsonProperty("touchpoints")
    @Schema(required = true)
    @NotNull
    @Valid
    private List<Touchpoint> touchpointList;


    @Schema(required = true)
    @NotNull
    @Valid
    private PageInfo pageInfo;
}