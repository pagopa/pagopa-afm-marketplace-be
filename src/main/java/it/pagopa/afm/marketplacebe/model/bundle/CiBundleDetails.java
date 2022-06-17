package it.pagopa.afm.marketplacebe.model.bundle;

import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class CiBundleDetails {
    private LocalDateTime validityDateTo;
    private List<CiBundleAttribute> attributes;
}
