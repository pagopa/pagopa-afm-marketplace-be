package it.pagopa.afm.marketplacebe.model.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

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
