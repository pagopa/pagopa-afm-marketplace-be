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

@UtilityClass
public class TestUtil {

    static public Bundle getMockBundle() {
        return Bundle.builder()
                .id(UUID.randomUUID().toString())
                .idPsp("test_psp")
                .name("name")
                .description("description")
                .paymentAmount(100L)
                .minPaymentAmount(1L)
                .maxPaymentAmount(1000L)
                .paymentMethod(PaymentMethod.valueOf("PO"))
                .touchpoint(Touchpoint.valueOf("IO"))
                .type(BundleType.valueOf("PRIVATE"))
                .transferCategoryList(List.of("TEST"))
                .validityDateFrom(null)
                .validityDateTo(null)
                .insertedDate(LocalDateTime.now().minusDays(2))
                .lastUpdatedDate(LocalDateTime.now().minusDays(1))
                .build();
    }

    static public CiBundle getMockCiBundle() {
        return CiBundle.builder()
                .id(UUID.randomUUID().toString())
                .ciFiscalCode("ABCD")
                .validityDateTo(LocalDateTime.now())
                .insertedDate(LocalDateTime.now())
                .idBundle(UUID.randomUUID().toString())
                .attributes(Lists.newArrayList(getMockCiBundleAttribute()))
                .build();
    }

    private static CiBundleAttribute getMockCiBundleAttribute() {
        return CiBundleAttribute.builder()
                .id(UUID.randomUUID().toString())
                .maxPaymentAmount(100L)
                .insertedDate(LocalDateTime.now())
                .transferCategory("E")
                .transferCategoryRelation(TransferCategoryRelation.EQUAL)
                .build();
    }

    static public BundleRequest getMockBundleRequest() {
        return BundleRequest.builder()
                .ciFiscalCode("ABCD")
                .idPsp("test_psp")
                .idBundle(UUID.randomUUID().toString())
                .insertedDate(LocalDateTime.now())
                .ciBundleAttributes(Lists.newArrayList(getMockCiBundleAttribute()))
                .build();
    }

}
