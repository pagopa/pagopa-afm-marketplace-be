package it.pagopa.afm.marketplacebe;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import lombok.experimental.UtilityClass;
import org.assertj.core.util.Lists;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import lombok.experimental.UtilityClass;
import org.modelmapper.ModelMapper;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;


@UtilityClass
public class TestUtil {

    public static String getMockIdPsp() {
        return "1234567890";
    }

    public static BundleRequest getMockBundleRequest() {
        List<String> transferCategoryList = Arrays.asList("taxonomy1", "taxonomy2");

        return BundleRequest.builder()
                .name("name")
                .description("description")
                .paymentAmount(100L)
                .minPaymentAmount(0L)
                .maxPaymentAmount(10000L)
                .paymentMethod(PaymentMethod.CP)
                .touchpoint(Touchpoint.IO)
                .type(BundleType.GLOBAL)
                .transferCategoryList(transferCategoryList)
                .validityDateFrom(LocalDateTime.now().plusDays(1))
                .validityDateTo(LocalDateTime.now().plusDays(8))
                .build();
    }

    public static Bundle getMockBundle() {
        ModelMapper modelMapper = new ModelMapper();
        Bundle bundle = modelMapper.map(getMockBundleRequest(), Bundle.class);
        bundle.setId("cbfbc9c6-6c0b-429e-83ca-30ef453504f8");
        bundle.setIdPsp(getMockIdPsp());
        bundle.setInsertedDate(LocalDateTime.now());
        bundle.setLastUpdatedDate(LocalDateTime.now());
        return bundle;
    }

    public static List<Bundle> getMockBundleSameConfiguration() {
        Bundle bundle = getMockBundle();
        return List.of(bundle);
    }

    public static List<Bundle> getMockBundleSameConfigurationDifferentPaymentAmountRange() {
        List<String> transferCategoryList = Arrays.asList("taxonomy1");

        Bundle bundle1 = getMockBundle();
        bundle1.setMinPaymentAmount(10001L);
        bundle1.setMaxPaymentAmount(20000L);

        Bundle bundle2 = getMockBundle();
        bundle2.setMinPaymentAmount(10001L);
        bundle2.setMaxPaymentAmount(20000L);
        bundle2.setTransferCategoryList(transferCategoryList);

        return List.of(bundle1, bundle2);
    }

    public static List<Bundle> getMockBundleWithoutValidityDateTo() {
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        return List.of(bundle);
    }


}
