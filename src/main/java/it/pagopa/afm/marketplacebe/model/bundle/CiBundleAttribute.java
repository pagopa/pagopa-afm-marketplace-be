package it.pagopa.afm.marketplacebe.model.bundle;

import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class CiBundleAttribute {
    private Long maxPaymentAmount;
    private String transferCategory;
    private TransferCategoryRelation transferCategoryRelation;
    private LocalDate validityDateTo;
    private LocalDateTime insertedDate;
}
