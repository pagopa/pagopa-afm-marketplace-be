package it.pagopa.afm.marketplacebe.model.bundle;

import lombok.*;

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
    private List<CiBundleAttribute> attributes;
}
