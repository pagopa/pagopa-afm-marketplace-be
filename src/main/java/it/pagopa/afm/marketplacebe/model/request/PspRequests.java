package it.pagopa.afm.marketplacebe.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class PspRequests {

    @JsonProperty("requests")
    @Schema(required = true)
    @NotNull
    @Valid
    private List<PspBundleRequest> requestsList;


    @JsonProperty("pageInfo")
    @Schema(required = true)
    @NotNull
    @Valid
    private PageInfo pageInfo;
}
