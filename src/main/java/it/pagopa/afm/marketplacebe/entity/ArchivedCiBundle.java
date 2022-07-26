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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "cibundles")
@Getter
@Setter
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class ArchivedCiBundle {

    @Id
    @NotBlank
    private String id;

    @PartitionKey
    @NotBlank
    private String ciFiscalCode;

    @NotNull
    private String idBundle;

    @Valid
    private List<CiBundleAttribute> attributes;

    private LocalDate validityDateFrom;

    private LocalDate validityDateTo;

    private LocalDateTime insertedDate;

}
