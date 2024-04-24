package it.pagopa.afm.marketplacebe.model.bundle;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class CiBundleDetails {
    private LocalDate validityDateFrom;
    private LocalDate validityDateTo;
    private String idCIBundle;
    private String ciTaxCode;
    private List<CiBundleAttribute> attributes;
}
