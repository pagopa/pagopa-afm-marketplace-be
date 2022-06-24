package it.pagopa.afm.marketplacebe.model.offer;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class CiBundleId {

    @JsonProperty("idCiBundle")
    private String id;
}
