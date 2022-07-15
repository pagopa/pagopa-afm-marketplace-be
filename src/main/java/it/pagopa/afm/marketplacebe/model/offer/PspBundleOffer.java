package it.pagopa.afm.marketplacebe.model.offer;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class PspBundleOffer {

    @JsonProperty("idBundleOffer")
    private String id;
    private String idBundle;
    private String ciFiscalCode;
    private LocalDateTime acceptedDate;
    private LocalDateTime rejectionDate;
    private LocalDateTime insertedDate;

}
