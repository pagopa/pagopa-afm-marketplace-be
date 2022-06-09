package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedDate;

import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "cibundles")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class CiBundle {

    @Id
    private Long id;

    @PartitionKey
    private String ciFiscalCode;

    private Bundle bundle;

    private List<CiBundleAttribute> attributes;

    private LocalDateTime validityDateTo;

    @CreatedDate
    private LocalDateTime insertedDate;

}
