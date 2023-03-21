package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
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

    private LocalDateTime insertedDate;
}
