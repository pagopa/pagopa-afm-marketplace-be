package it.pagopa.afm.marketplacebe.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;
import java.util.List;

// TODO delete me

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class PspBundleRequest {
    @JsonProperty("idBundleRequest")
    @NotNull
    private String id;

    @NotBlank
    private String idBundle;

    @NotNull
    private String ciFiscalCode;

    private LocalDateTime acceptedDate;

    private LocalDateTime rejectionDate;

    @Valid
    private List<PspCiBundleAttribute> ciBundleAttributes;

}
