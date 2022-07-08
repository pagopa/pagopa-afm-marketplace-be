package it.pagopa.afm.marketplacebe.model.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class CiBundleSubscriptionRequest {

    @NotBlank
    private String idBundle;

    @JsonProperty("attributes")
    @Valid
    private List<CiBundleAttributeModel> ciBundleAttributeModelList;
}
