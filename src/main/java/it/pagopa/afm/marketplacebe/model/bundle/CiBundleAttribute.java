package it.pagopa.afm.marketplacebe.model.bundle;

import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import lombok.*;

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
    private LocalDateTime insertedDate;
}
