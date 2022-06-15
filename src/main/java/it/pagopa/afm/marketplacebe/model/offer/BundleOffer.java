package it.pagopa.afm.marketplacebe.model.offer;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleOffer {

    private String idBundleOffer;
    private String idBundle;
    private String ciFiscalCode;
    private LocalDateTime acceptedDate;
    private LocalDateTime rejectionDate;
    private LocalDateTime insertedDate;

}
