package it.pagopa.afm.marketplacebe.model.offer;

import lombok.*;

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
