package it.pagopa.afm.marketplacebe.model.bundle;

import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleDetailsAttributes {

    private LocalDate validityDateFrom;
    private LocalDate validityDateTo;

    @NotNull
    private LocalDateTime insertedDate;

    @Valid
    @NotNull
    private List<BundleAttribute> attributes;

}
