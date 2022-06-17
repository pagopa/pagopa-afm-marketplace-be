package it.pagopa.afm.marketplacebe.model.bundle;

import com.fasterxml.jackson.annotation.JsonProperty;
import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BundleAttribute {

    @JsonProperty("idBundleAttribute")
    @NotNull
    private String id;

    private Long maxPaymentAmount;

    private String transferCategory;

    private TransferCategoryRelation transferCategoryRelation;

    private LocalDateTime validityDateTo;

    @NotNull
    private LocalDateTime insertedDate;

}
