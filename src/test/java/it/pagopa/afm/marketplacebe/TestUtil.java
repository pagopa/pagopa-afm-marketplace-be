package it.pagopa.afm.marketplacebe;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JSR310Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.*;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.offer.PspBundleOffer;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.model.request.CiBundleSubscriptionRequest;
import it.pagopa.afm.marketplacebe.model.request.PspBundleRequest;
import it.pagopa.afm.marketplacebe.model.request.PspRequests;
import lombok.experimental.UtilityClass;
import org.assertj.core.util.Lists;
import org.modelmapper.ModelMapper;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;


@UtilityClass
public class TestUtil {

    private final static String MOCK_ID_PSP = "1234567890";
    private final static String MOCK_CI_FISCAL_CODE = "fiscalCode";
    private final static String MOCK_ID_BUNDLE = "cbfbc9c6-6c0b-429e-83ca-30ef453504f8";

    public static String getMockIdPsp() {
        return MOCK_ID_PSP;
    }
    public static String getMockCiFiscalCode() {
        return MOCK_CI_FISCAL_CODE;
    }
    public static String getMockIdBundle() { return MOCK_ID_BUNDLE; }

    /**
     * @param object to map into the Json string
     * @return object as Json string
     * @throws JsonProcessingException if there is an error during the parsing of the object
     */
    public String toJson(Object object) throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        return mapper.writeValueAsString(object);
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
        bundle.setId(getMockIdBundle());
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

    public static CiBundleSubscriptionRequest getMockCiBundleSubscriptionRequest() {
        return CiBundleSubscriptionRequest.builder()
                .idBundle(getMockIdBundle())
                .ciBundleAttributeModelList(List.of(getMockCiBundleAttributeModel()))
                .build();
    }

    private static CiBundleAttributeModel getMockCiBundleAttributeModel() {
        return CiBundleAttributeModel.builder()
                .maxPaymentAmount(100L)
                .transferCategory("taxonomy1")
                .transferCategoryRelation(TransferCategoryRelation.EQUAL)
                .build();
    }

    public static CiFiscalCodeList getMockCiFiscalCodeList() {
        return CiFiscalCodeList.builder()
                .ciFiscalCodeList(List.of(getMockCiFiscalCode()))
                .build();
    }

    public static CiBundleDetails getMockCiBundleDetails() {
        ModelMapper modelMapper = new ModelMapper();
        CiBundleAttribute attributeE = getMockCiBundleAttribute();
        it.pagopa.afm.marketplacebe.model.bundle.CiBundleAttribute attributeM = modelMapper.map(attributeE, it.pagopa.afm.marketplacebe.model.bundle.CiBundleAttribute.class);
        return CiBundleDetails.builder()
                .validityDateFrom(LocalDate.now().minusDays(7))
                .validityDateTo(LocalDate.now().plusDays(7))
                .attributes(List.of(attributeM))
                .build();
    }

    public static PspBundleDetails getMockPspBundleDetails() {
        Bundle bundle = getMockBundle();
        ModelMapper modelMapper = new ModelMapper();
        return modelMapper.map(bundle, PspBundleDetails.class);
    }

    public static Bundles getMockBundles() {
        List<PspBundleDetails> bundleList = List.of(getMockPspBundleDetails());
        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(bundleList.size())
                .totalPages(1)
                .build();
        return Bundles.builder()
                .bundleDetailsList(bundleList)
                .pageInfo(pageInfo)
                .build();
    }

    public static BundleResponse getMockBundleResponse() {
        return BundleResponse.builder()
                .idBundle(getMockIdBundle())
                .build();
    }

    public static PspBundleOffer getMockPspBundleOffer() {
        return PspBundleOffer.builder()
                .id(UUID.randomUUID().toString())
                .idBundle(getMockIdBundle())
                .ciFiscalCode(getMockCiFiscalCode())
                .acceptedDate(null)
                .rejectionDate(null)
                .insertedDate(LocalDateTime.now())
                .build();
    }

    public static BundleOffers getMockBundleOffers() {
        List<PspBundleOffer> offers = List.of(getMockPspBundleOffer());
        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(offers.size())
                .totalPages(1)
                .build();
        return BundleOffers.builder()
                .offers(offers)
                .pageInfo(pageInfo)
                .build();
    }

    public static BundleOffered getMockBundleOffered() {
        return BundleOffered.builder()
                .ciFiscalCode(getMockCiFiscalCode())
                .idBundleOffer(UUID.randomUUID().toString())
                .build();
    }

    public static PspBundleRequest getMockPspBundleRequest() {
        return PspBundleRequest.builder()
                .id(UUID.randomUUID().toString())
                .idBundle(getMockIdBundle())
                .ciFiscalCode(getMockCiFiscalCode())
                .acceptedDate(null)
                .rejectionDate(null)
                .ciBundleAttributes(Collections.emptyList())
                .build();
    }
    public static PspRequests getMockPspRequests() {
        List<PspBundleRequest> list = List.of(getMockPspBundleRequest());
        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(list.size())
                .totalPages(1)
                .build();

        return PspRequests.builder()
                .requestsList(list)
                .pageInfo(pageInfo)
                .build();
    }
}
