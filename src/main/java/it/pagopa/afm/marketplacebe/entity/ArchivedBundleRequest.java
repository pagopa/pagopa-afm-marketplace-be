package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "archivedbundlerequests")
@Getter
@Setter
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class ArchivedBundleRequest {

    @Id
    @NotNull
    private String id;

    @PartitionKey
    @NotNull
    @Size(max = 35)
    private String idPsp;

    @NotNull
    private String ciFiscalCode;

    @NotBlank
    private String idBundle;

    @Valid
    private List<CiBundleAttribute> ciBundleAttributes;

    private LocalDateTime acceptedDate;

    private LocalDateTime rejectionDate;

    @NotNull
    private LocalDateTime insertedDate;
}
