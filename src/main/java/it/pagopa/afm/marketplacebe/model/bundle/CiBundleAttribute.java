package it.pagopa.afm.marketplacebe.model.bundle;

import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
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
public class CiBundleAttribute {
  private Long maxPaymentAmount;
  private String transferCategory;
  private TransferCategoryRelation transferCategoryRelation;
  private LocalDateTime insertedDate;
}
