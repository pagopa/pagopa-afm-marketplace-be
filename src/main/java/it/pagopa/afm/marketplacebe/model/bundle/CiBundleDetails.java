package it.pagopa.afm.marketplacebe.model.bundle;

import java.time.LocalDate;
import java.util.List;
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
public class CiBundleDetails {
  private LocalDate validityDateFrom;
  private LocalDate validityDateTo;
  private String idBundle;
  private String idCIBundle;
  private String ciTaxCode;
  private List<CiBundleAttribute> attributes;
}
