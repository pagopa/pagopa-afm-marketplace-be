package it.pagopa.afm.marketplacebe.model.bundle;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class PspBundleDetails {

    @JsonProperty("idBundle")
    private String id;
    private String name;
    private String description;
    private Long paymentAmount;
    private Long minPaymentAmount;
    private Long maxPaymentAmount;
    private String paymentMethod;
    private String touchpoint;
    private String type;
    private List<String> transferCategoryList;
    private LocalDate validityDateFrom;
    private LocalDate validityDateTo;
    private LocalDateTime insertedDate;
    private LocalDateTime lastUpdatedDate;

}
