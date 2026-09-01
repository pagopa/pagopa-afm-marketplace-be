package it.pagopa.afm.marketplacebe.model.bundle;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
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
public class BundleDetailsAttributes {

  private LocalDate validityDateFrom;
  private LocalDate validityDateTo;

  @NotNull private LocalDateTime insertedDate;

  @Valid @NotNull private List<BundleAttribute> attributes;
}
