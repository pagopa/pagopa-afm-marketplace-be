package it.pagopa.afm.marketplacebe.model.request;

import lombok.*;
import java.time.LocalDateTime;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class CiBundleRequest {

    private String idBundleRequest;
    private String idBundle;
    private String idPsp;

    private LocalDateTime acceptedDate;
    private LocalDateTime rejectionDate;
    private LocalDateTime insertedDate;

    private List<CiBundleAttribute> ciBundleAttributes;
}