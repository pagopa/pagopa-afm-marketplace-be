package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;

import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "bundlerequests")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class BundleRequest {

    @Id
    private Long id;

    @PartitionKey
    private String idPsp;

    private String ciFiscalCode;

    private List<CiBundleAttribute> ciBundleAttributes;

    private LocalDateTime acceptedDate;
    private LocalDateTime rejectionDate;

    @CreatedDate
    private LocalDateTime insertedDate;

}
