package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;

import java.time.LocalDateTime;

@Container(containerName = "archiviedbundleoffers")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class ArchivedBundleOffer {

    @Id
    @NonNull
    private String id;

    @PartitionKey
    private String ciFiscalCode;

    private String idPsp;

    private String idBundle;

    private LocalDateTime acceptedDate;
    private LocalDateTime rejectionDate;

    @CreatedDate
    private LocalDateTime insertedDate;

}