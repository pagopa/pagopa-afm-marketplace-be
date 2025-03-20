package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.GeneratedValue;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedDate;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "bundles")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class Bundle {

    @Id
    @GeneratedValue
    private String id;

    @PartitionKey
    @NotNull
    @Size(max = 35)
    private String idPsp;

    @NotNull
    private String idChannel;

    @NotNull
    private String idBrokerPsp;

    private Boolean cart;
    
    private String idCdi;

    @NotNull
    private String abi;
    
    @NotNull
    private String pspBusinessName;

    private String urlPolicyPsp;

    private Boolean digitalStamp;

    // true if bundle must be used only for digital stamp
    private Boolean digitalStampRestriction;

    private String name;
    private String description;

    private Long paymentAmount;
    private Long minPaymentAmount;
    private Long maxPaymentAmount;

    private String paymentType;

    private String touchpoint;

    private BundleType type;

    private List<String> transferCategoryList;

    private LocalDate validityDateFrom;

    private LocalDate validityDateTo;

    @CreatedDate
    private LocalDateTime insertedDate;

    @LastModifiedDate
    private LocalDateTime lastUpdatedDate;

    private Boolean onUs;

}
