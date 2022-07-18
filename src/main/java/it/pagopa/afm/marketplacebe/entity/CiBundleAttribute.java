package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.GeneratedValue;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;

import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class CiBundleAttribute {

    @Id
    @GeneratedValue
    @NotNull
    private String id;

    private Long maxPaymentAmount;

    private String transferCategory;

    private TransferCategoryRelation transferCategoryRelation;

    @NotNull
    private LocalDateTime insertedDate;

}
