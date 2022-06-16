package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.GeneratedValue;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedDate;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "bundles")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class Bundle{

    @Id
    @GeneratedValue
    private String idBundle;

    @PartitionKey
    @NotNull
    @Size(max = 35)
    private String idPsp;

    private String name;
    private String description;

    private Long paymentAmount;
    private Long minPaymentAmount;
    private Long maxPaymentAmount;

    private PaymentMethod paymentMethod;

    private Touchpoint touchpoint;

    private BundleType type;

    private List<String> transferCategoryList;

    private LocalDateTime validityDateFrom;
    private LocalDateTime validityDateTo;

    @CreatedDate
    private LocalDateTime insertedDate;

    @LastModifiedDate
    private LocalDateTime lastUpdatedDate;

}
