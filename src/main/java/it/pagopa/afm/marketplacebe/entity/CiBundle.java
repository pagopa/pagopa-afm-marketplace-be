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

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "cibundles")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CiBundle {

    @Id
    @GeneratedValue
    @NotBlank
    private String id;

    @PartitionKey
    @NotBlank
    @Size(max = 35)
    private String ciFiscalCode;

    @NotNull
    @Valid
    private Bundle bundle;

    @Valid
    private List<CiBundleAttribute> attributes;

    private LocalDateTime validityDateTo;

    @CreatedDate
    private LocalDateTime insertedDate;

}
