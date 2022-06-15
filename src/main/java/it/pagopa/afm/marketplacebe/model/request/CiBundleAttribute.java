package it.pagopa.afm.marketplacebe.model.request;

import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;

import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;

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
