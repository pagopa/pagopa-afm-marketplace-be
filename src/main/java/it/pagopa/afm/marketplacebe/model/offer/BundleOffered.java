package it.pagopa.afm.marketplacebe.model.offer;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleOffered {

    private String ciFiscalCode;

    private String idBundleOffer;
}
