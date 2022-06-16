package it.pagopa.afm.marketplacebe.model.bundle;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleResponse {
    private String idBundle;
}
