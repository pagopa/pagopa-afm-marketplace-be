package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.GeneratedValue;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "bundlerequests")
@Getter
@Setter
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class BundleRequestEntity {

    @Id
    @GeneratedValue
    @NotNull
    private String id;

    @PartitionKey
    @NotNull
    @Size(max = 35)
    private String idPsp;

    @NotBlank
    private String idBundle;

    @NotNull
    private String ciFiscalCode;

    @Valid
    private List<CiBundleAttribute> ciBundleAttributes;

    @Schema(description = "the start date of the bundle if accepted")
    private LocalDate validityDateFrom;

    @Schema(description = "the end date of the bundle if accepted")
    private LocalDate validityDateTo;

    private LocalDateTime acceptedDate;

    private LocalDateTime rejectionDate;

    @CreatedDate
    @NotNull
    private LocalDateTime insertedDate;

}
