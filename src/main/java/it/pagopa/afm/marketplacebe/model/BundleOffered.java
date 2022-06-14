package it.pagopa.afm.marketplacebe.model;


import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleOffered {

    private String ciFiscalCode;

    private String idBundleOffer;
}
