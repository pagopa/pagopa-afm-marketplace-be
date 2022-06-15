package it.pagopa.afm.marketplacebe.model.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotBlank;
import java.util.List;

@Getter
@Setter
public class CiBundleSubscriptionRequest {

    @NotBlank
    private String idBundle;

    @JsonProperty("attributes")
    List<CiBundleAttribute> ciBundleAttributeList;
}
