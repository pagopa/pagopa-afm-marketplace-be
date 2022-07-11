package it.pagopa.afm.marketplacebe.model.bundle;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class CiBundleInfo {

    @JsonProperty("idBundle")
    private String id;
    private String idCiBundle;
    private String idPsp;
    private String name;
    private String description;
    private Long paymentAmount;
    private Long minPaymentAmount;
    private Long maxPaymentAmount;
    private String paymentMethod;
    private String touchpoint;
    private String type;
    private List<String> transferCategoryList;
    private LocalDateTime validityDateFrom;
    private LocalDateTime validityDateTo;
    private LocalDateTime insertedDate;
    private LocalDateTime lastUpdatedDate;

}
