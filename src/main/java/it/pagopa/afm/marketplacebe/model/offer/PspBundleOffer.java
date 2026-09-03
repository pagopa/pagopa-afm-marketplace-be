package it.pagopa.afm.marketplacebe.model.offer;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.LocalDateTime;
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
public class PspBundleOffer {

  @JsonProperty("idBundleOffer")
  private String id;

  private String idBundle;
  private String ciFiscalCode;
  private LocalDateTime acceptedDate;
  private LocalDateTime rejectionDate;
  private LocalDateTime insertedDate;
}
