package it.pagopa.afm.marketplacebe.model.bundle;

import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleRequest {
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
}
