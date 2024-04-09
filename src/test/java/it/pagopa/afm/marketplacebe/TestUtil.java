package it.pagopa.afm.marketplacebe;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.assertj.core.util.Lists;
import org.modelmapper.ModelMapper;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundleOffer;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.entity.PaymentType;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.BundleAttributeResponse;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetailsAttributes;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetailsForCi;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.bundle.BundleResponse;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleDetails;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleInfo;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundles;
import it.pagopa.afm.marketplacebe.model.bundle.PspBundleDetails;
import it.pagopa.afm.marketplacebe.model.offer.BundleCiOffers;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiBundleId;
import it.pagopa.afm.marketplacebe.model.offer.CiBundleOffer;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.offer.PspBundleOffer;
import it.pagopa.afm.marketplacebe.model.request.BundleRequestId;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.model.request.CiBundleRequest;
import it.pagopa.afm.marketplacebe.model.request.CiBundleSubscriptionRequest;
import it.pagopa.afm.marketplacebe.model.request.CiRequests;
import it.pagopa.afm.marketplacebe.model.request.PspBundleRequest;
import it.pagopa.afm.marketplacebe.model.request.PspRequests;
import lombok.experimental.UtilityClass;


@UtilityClass
public class TestUtil {

  private final static String MOCK_ID_PSP = "1234567890";
  private final static String MOCK_CI_FISCAL_CODE = "fiscalCode";
  private final static String MOCK_ID_BUNDLE = "cbfbc9c6-6c0b-429e-83ca-30ef453504f8";

  private final static String MOCK_ID_OFFER = "acfbc9c6-6c0b-429e-83ca-30ef453504f8";

  private final static String MOCK_ID_PAYMENT_TYPE = "76c16af5-241b-4b9d-bda2-0b5f6b427a4c";

  private final static String MOCK_PSP_BUSINESS_NAME = "MockBusinessName";

  public static String getMockIdPsp() {
    return MOCK_ID_PSP;
  }

  public static String getMockCiFiscalCode() {
    return MOCK_CI_FISCAL_CODE;
  }

  public static String getMockIdBundle() {
    return MOCK_ID_BUNDLE;
  }

  public static String getMockBundleOfferId() {
    return MOCK_ID_OFFER;
  }

  public static String getMockPspBusinessName() {
    return MOCK_PSP_BUSINESS_NAME;
  }

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
        .idChannel("idChannel")
        .idBrokerPsp("idBrokerPsp")
        .idCdi(null)
        .abi("abi")
        .name("name")
        .pspBusinessName("business name")
        .description("description")
        .paymentAmount(100L)
        .minPaymentAmount(0L)
        .maxPaymentAmount(10000L)
        .paymentType("CP")
        .digitalStamp(false)
        .digitalStampRestriction(false)
        .touchpoint("IO")
        .type(BundleType.GLOBAL)
        .transferCategoryList(transferCategoryList)
        .validityDateFrom(LocalDate.now().plusDays(1))
        .validityDateTo(LocalDate.now().plusDays(8))
        .digitalStamp(Boolean.TRUE)
        .digitalStampRestriction(Boolean.FALSE)
        .build();
  }

  public static BundleRequest getMockBundleRequestWithPaymentTypeNull() {
    List<String> transferCategoryList = Arrays.asList("taxonomy1", "taxonomy2");

    return BundleRequest.builder()
        .idChannel("idChannel")
        .idBrokerPsp("idBrokerPsp")
        .idCdi("idCdi")
        .abi("abi")
        .name("name")
        .description("description")
        .paymentAmount(100L)
        .minPaymentAmount(0L)
        .maxPaymentAmount(10000L)
        .paymentType(null)
        .digitalStamp(false)
        .digitalStampRestriction(false)
        .touchpoint("IO")
        .type(BundleType.GLOBAL)
        .transferCategoryList(transferCategoryList)
        .validityDateFrom(LocalDate.now().plusDays(1))
        .validityDateTo(LocalDate.now().plusDays(8))
        .build();
  }

  public static List<BundleRequest> getMockBundleRequestList() {

    List<BundleRequest> bundleRequestList = new ArrayList<>();

    List<String> transferCategoryList = Arrays.asList("taxonomy1", "taxonomy2");

    bundleRequestList.add(BundleRequest.builder()
        .idChannel("idChannel1")
        .idBrokerPsp("idBrokerPsp1")
        .idCdi("idCdi")
        .abi("abi")
        .name("name")
        .pspBusinessName("business name 0")
        .description("description")
        .paymentAmount(100L)
        .minPaymentAmount(0L)
        .maxPaymentAmount(10000L)
        .paymentType("CP")
        .digitalStamp(false)
        .digitalStampRestriction(false)
        .touchpoint("IO")
        .type(BundleType.GLOBAL)
        .transferCategoryList(transferCategoryList)
        .validityDateFrom(LocalDate.now().plusDays(1))
        .validityDateTo(LocalDate.now().plusDays(8))
        .build());
    bundleRequestList.add(BundleRequest.builder()
        .idChannel("idChannel2")
        .idBrokerPsp("idBrokerPsp2")
        .idCdi("idCdi")
        .abi("abi")
        .name("name")
        .pspBusinessName("business name 1")
        .description("description")
        .paymentAmount(200L)
        .minPaymentAmount(0L)
        .maxPaymentAmount(20000L)
        .paymentType("CP")
        .digitalStamp(false)
        .digitalStampRestriction(false)
        .touchpoint("IO")
        .type(BundleType.GLOBAL)
        .transferCategoryList(transferCategoryList)
        .validityDateFrom(null)
        .validityDateTo(null)
        .build());
    bundleRequestList.add(BundleRequest.builder()
        .idChannel("idChannel3")
        .idBrokerPsp("idBrokerPsp3")
        .idCdi("idCdi")
        .abi("abi")
        .name("name")
        .pspBusinessName("business name 2")
        .description("description")
        .paymentAmount(300L)
        .minPaymentAmount(0L)
        .maxPaymentAmount(30000L)
        .paymentType("CP")
        .digitalStamp(false)
        .digitalStampRestriction(false)
        .touchpoint("IO")
        .type(BundleType.GLOBAL)
        .transferCategoryList(transferCategoryList)
        .validityDateFrom(LocalDate.now().plusDays(1))
        .validityDateTo(null)
        .build());

    return bundleRequestList;
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
    modelMapper.getConfiguration().setAmbiguityIgnored(true);
    Bundle bundle = modelMapper.map(getMockBundleRequest(), Bundle.class);
    bundle.setId(getMockIdBundle());
    bundle.setIdPsp(getMockIdPsp());
    bundle.setInsertedDate(LocalDateTime.now());
    bundle.setLastUpdatedDate(LocalDateTime.now());
    bundle.setPaymentType("CP");
    return bundle;
  }

  public static List<Bundle> getMockBundleList() {
    List<Bundle> bundleList = Arrays.asList(
        Bundle.builder()
        .id(getMockIdBundle())
        .idPsp(getMockIdPsp())
        .insertedDate(LocalDateTime.now())
        .lastUpdatedDate(LocalDateTime.now())
        .paymentType("CP")
        .build(),
        Bundle.builder()
        .id(getMockIdBundle())
        .idPsp(getMockIdPsp())
        .insertedDate(LocalDateTime.now())
        .lastUpdatedDate(LocalDateTime.now())
        .paymentType("CP")
        .build(),
        Bundle.builder()
        .id(getMockIdBundle())
        .idPsp(getMockIdPsp())
        .insertedDate(LocalDateTime.now())
        .lastUpdatedDate(LocalDateTime.now())
        .paymentType("CP")
        .build());
    return bundleList;
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

  public static CiBundleInfo getMockCiBundleInfo() {
    Bundle bundle = getMockBundle();
    ModelMapper mapper = new ModelMapper();
    mapper.map(bundle, CiBundleInfo.class);
    return CiBundleInfo.builder()
        .build();
  }

  public static CiBundles getMockCiBundles() {
    List<CiBundleInfo> list = List.of(getMockCiBundleInfo());

    PageInfo pageInfo = PageInfo.builder()
        .itemsFound(list.size())
        .totalPages(1)
        .build();

    return CiBundles.builder()
        .bundleDetailsList(list)
        .pageInfo(pageInfo)
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

  public static CiBundleAttributeModel getMockBundleAttribute() {
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

  public static CiBundleAttributeModel getMockCiBundleAttributeModel() {
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

  public static BundleDetailsForCi getMockBundleDetailsForCi() {
    Bundle bundle = getMockBundle();
    ModelMapper modelMapper = new ModelMapper();
    return modelMapper.map(bundle, BundleDetailsForCi.class);
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

  public static List<BundleResponse> getMockBundleResponseList() {
    return Arrays.asList(BundleResponse.builder()
        .idBundle(getMockIdBundle())
        .build(),
        BundleResponse.builder()
        .idBundle(getMockIdBundle())
        .build(),
        BundleResponse.builder()
        .idBundle(getMockIdBundle())
        .build());
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

  public static BundleOffer getMockBundleOffer() {
    return BundleOffer.builder()
        .id(getMockBundleOfferId())
        .ciFiscalCode(getMockCiFiscalCode())
        .idPsp(getMockIdPsp())
        .idBundle(getMockIdBundle())
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

  public static List<BundleOffer> getMockBundleOfferList() {
    return List.of(
        getMockBundleOffer()
        );
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

  public static BundleDetailsAttributes getMockBundleDetailsAttributes() {
    CiBundle ciBundle = getMockCiBundle();
    ModelMapper modelMapper = new ModelMapper();
    return modelMapper.map(ciBundle, BundleDetailsAttributes.class);
  }

  public static BundleAttributeResponse getMockBundleAttributeResponse() {
    return BundleAttributeResponse.builder()
        .idBundleAttribute(UUID.randomUUID().toString())
        .build();
  }

  public static CiBundleRequest getMockCiBundleRequest() {
    return CiBundleRequest.builder()
        .id(UUID.randomUUID().toString())
        .idBundle(getMockIdBundle())
        .idPsp(getMockIdPsp())
        .acceptedDate(null)
        .rejectionDate(null)
        .insertedDate(LocalDateTime.now())
        .ciBundleAttributeModels(Collections.emptyList())
        .build();
  }

  public static CiRequests getMockCiRequests() {
    List<CiBundleRequest> list = List.of(getMockCiBundleRequest());
    PageInfo pageInfo = PageInfo.builder()
        .itemsFound(list.size())
        .totalPages(1)
        .build();

    return CiRequests.builder()
        .requestsList(list)
        .pageInfo(pageInfo)
        .build();
  }

  public static BundleRequestId getMockBundleRequestId() {
    return BundleRequestId.builder()
        .idBundleRequest(UUID.randomUUID().toString())
        .build();
  }

  public static CiBundleOffer getMockCiBundleOffer() {
    return CiBundleOffer.builder()
        .idBundle(getMockIdBundle())
        .idPsp(getMockIdPsp())
        .acceptedDate(null)
        .rejectionDate(null)
        .insertedDate(LocalDateTime.now())
        .build();
  }

  public static BundleCiOffers getMockBundleCiOffers() {
    List<CiBundleOffer> list = List.of(getMockCiBundleOffer());
    PageInfo pageInfo = PageInfo.builder()
        .itemsFound(list.size())
        .totalPages(1)
        .build();
    return BundleCiOffers.builder()
        .offers(list)
        .pageInfo(pageInfo)
        .build();
  }

  public static CiBundleId getMockCiBundleId() {
    return CiBundleId.builder()
        .id(UUID.randomUUID().toString())
        .build();
  }

  public static ArchivedBundleOffer getMockArchivedBundleOffer(BundleOffer offer) {
    return ArchivedBundleOffer.builder()
        .idBundle(offer.getIdBundle())
        .idPsp(offer.getIdPsp())
        .acceptedDate(offer.getAcceptedDate())
        .rejectionDate(offer.getRejectionDate())
        .ciFiscalCode(offer.getCiFiscalCode())
        .id(offer.getId())
        .build();
  }

  public static Touchpoint getMockTouchpoint() {
    return Touchpoint.builder()
        .id(UUID.randomUUID().toString())
        .name("IO")
        .createdDate(LocalDateTime.now())
        .build();
  }

  public static Touchpoint getMockTouchpoint(String name) {
    return Touchpoint.builder()
        .id(UUID.randomUUID().toString())
        .name(name)
        .createdDate(LocalDateTime.now())
        .build();
  }

  public static Iterable<Touchpoint> getMockTouchpoints() {
    return List.of(
        getMockTouchpoint("IO"),
        getMockTouchpoint("CHECKOUT"),
        getMockTouchpoint("WISP")
        );
  }

  public static it.pagopa.afm.marketplacebe.entity.Touchpoint getMockTouchpointRequest() {
    return it.pagopa.afm.marketplacebe.entity.Touchpoint.builder()
        .name("IO")
        .build();
  }

  public static PaymentType getMockPaymentType(){
    return PaymentType.builder()
        .id(MOCK_ID_PAYMENT_TYPE)
        .name("CP")
        .createdDate(LocalDateTime.now()).build();
  }

  public static List<PaymentType> getMockPaymentTypeList() {
    return List.of(getMockPaymentType());
  }

  public static List<PaymentType> getMockPaymentTypeListForCreate() {
    return List.of(getMockPaymentType());
  }
}
