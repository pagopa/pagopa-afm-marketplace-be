package it.pagopa.afm.marketplacebe.model.bundle;

import com.fasterxml.jackson.annotation.JsonSetter;
import com.fasterxml.jackson.annotation.Nulls;
import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class BundleRequest {
    @NotNull
    private String idChannel;
    @NotNull
    private String idBrokerPsp;

    @Schema(description = "is the bundle valid for cart payments?",
            defaultValue = "true",
            required = false)
    @JsonSetter(nulls = Nulls.SKIP)
    private Boolean cart = true;

    private String idCdi;
    @NotNull
    private String abi;
    private String name;
    @NotNull
    private String pspBusinessName;
    private String urlPolicyPsp;
    private String description;
    private Long paymentAmount;
    private Long minPaymentAmount;
    private Long maxPaymentAmount;
    private String paymentType;
    private Boolean digitalStamp;
    private Boolean digitalStampRestriction;
    private String touchpoint;
    private BundleType type;
    private List<String> transferCategoryList;
    private LocalDate validityDateFrom;
    private LocalDate validityDateTo;
    @NotNull
    private Boolean onUs;
}
