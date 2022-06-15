package it.pagopa.afm.marketplacebe.model.request;

import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CiBundleAttribute {

    private Long maxPaymentAmount;
    private String transferCategory;
    private TransferCategoryRelation transferCategoryRelation;
}