package it.pagopa.afm.marketplacebe.model.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class CiBundleRequest {

    @JsonProperty("idBundleRequest")
    private String id;
    private String idBundle;
    private String idPsp;

    private LocalDateTime acceptedDate;
    private LocalDateTime rejectionDate;
    private LocalDateTime insertedDate;

    private List<CiBundleAttributeModel> ciBundleAttributeModels;
}
