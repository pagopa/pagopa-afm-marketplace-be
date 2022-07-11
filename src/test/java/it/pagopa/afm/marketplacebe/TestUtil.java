package it.pagopa.afm.marketplacebe;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import it.pagopa.afm.marketplacebe.entity.*;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.model.request.CiBundleSubscriptionRequest;
import lombok.experimental.UtilityClass;
import org.assertj.core.util.Lists;
import org.modelmapper.ModelMapper;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;


@UtilityClass
public class TestUtil {

    public static String getMockIdPsp() {
        return "1234567890";
    }

    public static String getMockCiFiscalCode() {
        return "fiscalCode";
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
                .validityDateFrom(LocalDate.now().plusDays(1))
                .validityDateTo(LocalDate.now().plusDays(8))
                .build();
    }

    public static BundleRequestEntity getMockBundleRequestEntity() {
        return BundleRequestEntity.builder()
                .id("112")
                .idPsp(getMockIdPsp())
                .ciFiscalCode(getMockCiFiscalCode())
                .idBundle(UUID.randomUUID().toString())
                .insertedDate(LocalDateTime.now())
                .validityDateFrom(LocalDate.now().plusDays(1))
                .validityDateTo(LocalDate.now().plusDays(8))
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
        List<String> transferCategoryList = List.of("taxonomy1");

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

    public static CiBundle getMockCiBundle() {
        return CiBundle.builder()
                .id(UUID.randomUUID().toString())
                .ciFiscalCode(getMockCiFiscalCode())
                .validityDateTo(LocalDate.now())
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

    public static BundleRequestEntity getMockBundleRequestE() {
        return BundleRequestEntity.builder()
                .ciFiscalCode(getMockCiFiscalCode())
                .idPsp(getMockIdPsp())
                .idBundle(UUID.randomUUID().toString())
                .insertedDate(LocalDateTime.now())
                .ciBundleAttributes(Lists.newArrayList(getMockCiBundleAttribute()))
                .build();
    }

    public static CiBundleAttributeModel getMockBundleAttribute(){
        return CiBundleAttributeModel.builder()
                .maxPaymentAmount(100L)
                .transferCategory("PO")
                .transferCategoryRelation(TransferCategoryRelation.EQUAL)
                .build();
    }

    public static CiBundleSubscriptionRequest getMockCiBundleSubscriptionRequest(){
        return CiBundleSubscriptionRequest
                .builder()
                .idBundle(UUID.randomUUID().toString())
                .ciBundleAttributeModelList(List.of(CiBundleAttributeModel.builder()
                        .transferCategoryRelation(TransferCategoryRelation.EQUAL)
                        .transferCategory("PO")
                        .maxPaymentAmount(100L).build()))
                .build();
    }


    public static CiFiscalCodeList getMockCiFiscalCodeList() {
        return CiFiscalCodeList.builder()
                .ciFiscalCodeList(Lists.newArrayList(getMockCiFiscalCode()))
                .build();
    }
}
