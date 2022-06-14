package it.pagopa.afm.marketplacebe.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class Requests {

    @JsonProperty("requests")
    @Schema(required = true)
    @NotNull
    @Valid
    private List<BundleRequest> requestsList;


    @JsonProperty("pageInfo")
    @Schema(required = true)
    @NotNull
    @Valid
    private PageInfo pageInfo;
}
