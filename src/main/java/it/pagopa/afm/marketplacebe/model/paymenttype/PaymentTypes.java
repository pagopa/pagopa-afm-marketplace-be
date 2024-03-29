package it.pagopa.afm.marketplacebe.model.paymenttype;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.entity.PaymentType;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class PaymentTypes {

    @JsonProperty("paymentTypes")
    @Schema(required = true)
    @NotNull
    @Valid
    private List<PaymentType> paymentTypeList;


    @Schema(required = true)
    @NotNull
    @Valid
    private PageInfo pageInfo;
}
