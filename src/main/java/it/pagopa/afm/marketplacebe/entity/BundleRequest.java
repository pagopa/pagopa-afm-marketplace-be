package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;

import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "bundlerequests")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BundleRequest {

    @Id
    private String id;

    @PartitionKey
    private String idPsp;

    private String idBundle;

    private String ciFiscalCode;

    private List<CiBundleAttribute> ciBundleAttributes;

    private LocalDateTime acceptedDate;
    private LocalDateTime rejectionDate;

    @CreatedDate
    private LocalDateTime insertedDate;

}
