package it.pagopa.afm.marketplacebe.entity;

import com.azure.spring.data.cosmos.core.mapping.Container;
import com.azure.spring.data.cosmos.core.mapping.GeneratedValue;
import com.azure.spring.data.cosmos.core.mapping.PartitionKey;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;

import java.time.LocalDateTime;
import java.util.List;

@Container(containerName = "cibundles")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class CiBundle {

    @Id
    @GeneratedValue
    private String id;

    @PartitionKey
    private String ciFiscalCode;

    private String idBundle;

    private List<CiBundleAttribute> attributes;

    private LocalDateTime validityDateTo;

    @CreatedDate
    private LocalDateTime insertedDate;

}
