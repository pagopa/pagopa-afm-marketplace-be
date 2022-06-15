package it.pagopa.afm.marketplacebe.entity;

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

    @Id
    @NotNull
    private Long id;

    private Long maxPaymentAmount;

    private String transferCategory;

    private TransferCategoryRelation transferCategoryRelation;

    private LocalDateTime validityDateTo;

    @CreatedDate
    @NotNull
    private LocalDateTime insertedDate;

}
