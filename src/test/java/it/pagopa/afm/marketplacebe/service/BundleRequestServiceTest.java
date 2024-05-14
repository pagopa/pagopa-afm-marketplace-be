package it.pagopa.afm.marketplacebe.service;

import com.azure.spring.data.cosmos.core.CosmosTemplate;
import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.config.MappingsConfiguration;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.TransferCategoryRelation;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.request.BundleRequestId;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.model.request.CiBundleSubscriptionRequest;
import it.pagopa.afm.marketplacebe.model.request.PublicBundleRequests;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static it.pagopa.afm.marketplacebe.TestUtil.getMockBundle;
import static it.pagopa.afm.marketplacebe.TestUtil.getMockBundleRequestE;
import static it.pagopa.afm.marketplacebe.TestUtil.getMockCiBundle;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest(classes = {BundleRequestService.class, MappingsConfiguration.class})
class BundleRequestServiceTest {

    private static final String FISCAL_CODE = "ciTaxCode";
    @MockBean
    private BundleRepository bundleRepository;

    @MockBean
    private CiBundleRepository ciBundleRepository;

    @MockBean
    private BundleRequestRepository bundleRequestRepository;

    @MockBean
    private ArchivedBundleRequestRepository archivedBundleRequestRepository;
    
    @MockBean
    private CosmosTemplate cosmosTemplate;

    @Autowired
    private BundleRequestService bundleRequestService;

    @ParameterizedTest
    @ValueSource(strings = {"PUBLIC"})
    void shouldCreateBundleRequest(String bundleType) {
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.fromValue(bundleType));
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());

        // Preconditions
        when(bundleRepository.findById(bundle.getId())).thenReturn(Optional.of(bundle));

        assertDoesNotThrow(() -> bundleRequestService.createBundleRequest(FISCAL_CODE, ciBundleSubscriptionRequest));

        verify(bundleRequestRepository).save(any());
    }

    @Test
    void shouldRaiseExceptionBundleTypeCreateBundleRequest_1() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.GLOBAL);

        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());

        // Preconditions
        when(bundleRepository.findById(bundle.getId())).thenReturn(Optional.of(bundle));

        AppException e = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(FISCAL_CODE, ciBundleSubscriptionRequest));

        assertEquals(HttpStatus.CONFLICT, e.getHttpStatus());
    }

    @Test
    void shouldRaiseExceptionBundleTypeCreateBundleRequest_2() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PUBLIC);

        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());
        ciBundleSubscriptionRequest.getCiBundleAttributeModelList().get(0).setMaxPaymentAmount(20000L);

        // Preconditions
        when(bundleRepository.findById(bundle.getId())).thenReturn(Optional.of(bundle));

        AppException e = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(FISCAL_CODE, ciBundleSubscriptionRequest));

        assertEquals(HttpStatus.BAD_REQUEST, e.getHttpStatus());
    }

    @Test
    void shouldRaiseExceptionBundleNotFoundCreateBundleRequest() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PRIVATE);

        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());

        // Preconditions
        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.empty());

        AppException e = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(FISCAL_CODE, ciBundleSubscriptionRequest));

        assertEquals(HttpStatus.NOT_FOUND, e.getHttpStatus());
    }

    @Test
    void shouldRaiseExceptionOnBundleAttributesCreateBundleRequest() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PUBLIC);

        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = CiBundleSubscriptionRequest.builder()
                .idBundle(bundle.getId())
                .ciBundleAttributeModelList(buildBadCIBundleAttributeModelList())
                .build();

        // Preconditions
        when(bundleRepository.findById(bundle.getId())).thenReturn(Optional.of(bundle));

        AppException e = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(FISCAL_CODE, ciBundleSubscriptionRequest));

        assertEquals(HttpStatus.BAD_REQUEST, e.getHttpStatus());
    }

    @Test
    void shouldRemoveBundleRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();

        // Preconditions
        when(bundleRequestRepository.findById(bundleRequest.getId())).thenReturn(Optional.of(bundleRequest));

        bundleRequestService.removeBundleRequest(bundleRequest.getCiFiscalCode(), bundleRequest.getId());
        verify(bundleRequestRepository, times(1)).delete(bundleRequest);
    }

    @Test
    void shouldRaiseBadRequestFiscalCodeBundleRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();
        String ciFiscalCode = "ABC";

        // Preconditions
        when(bundleRequestRepository.findById(bundleRequest.getId())).thenReturn(Optional.of(bundleRequest));

        removeBundleRequest_ko(ciFiscalCode, bundleRequest.getId(), HttpStatus.BAD_REQUEST);
    }

    @Test
    void shouldRaiseBadRequestNoRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();

        // Preconditions
        when(bundleRequestRepository.findById(bundleRequest.getId())).thenReturn(Optional.empty());

        removeBundleRequest_ko(TestUtil.getMockCiFiscalCode(), bundleRequest.getId(), HttpStatus.NOT_FOUND);
    }


    @Test
    void shouldGetRequestsByPsp_1() {
        List<BundleRequestEntity> bundleRequests = List.of(getMockBundleRequestE());

        when(bundleRequestRepository.findByIdPspAndFiscalCodeAndIdBundle(bundleRequests.get(0).getIdPsp(), null, null, 0 ,100))
                .thenReturn(bundleRequests);

        PublicBundleRequests requests = bundleRequestService.getPublicBundleRequests(bundleRequests.get(0).getIdPsp(),
                100, 0, null, null);

        assertEquals(bundleRequests.size(), requests.getRequestsList().size());
        assertEquals(bundleRequests.get(0).getIdBundle(), requests.getRequestsList().get(0).getIdBundle());
    }

    @Test
    void shouldGetRequestsByPsp_2() {
        List<BundleRequestEntity> bundleRequests = List.of(getMockBundleRequestE());

        when(bundleRequestRepository.findByIdPspAndFiscalCodeAndIdBundle(bundleRequests.get(0).getIdPsp(), bundleRequests.get(0).getCiFiscalCode(), null, 0,100))
                .thenReturn(bundleRequests);

        PublicBundleRequests requests = bundleRequestService.getPublicBundleRequests(bundleRequests.get(0).getIdPsp(),
                100, 0, bundleRequests.get(0).getCiFiscalCode(), null);

        assertEquals(bundleRequests.size(), requests.getRequestsList().size());
        assertEquals(bundleRequests.get(0).getIdBundle(), requests.getRequestsList().get(0).getIdBundle());
    }

    @Test
    void shouldGetRequestsByPsp_3() {
        List<BundleRequestEntity> bundleRequests = List.of(getMockBundleRequestE());

        when(bundleRequestRepository.findByIdPspAndFiscalCodeAndIdBundle(bundleRequests.get(0).getIdPsp(), bundleRequests.get(0).getCiFiscalCode(),
                        bundleRequests.get(0).getIdBundle(), 0,100)).thenReturn(bundleRequests);

        PublicBundleRequests requests = bundleRequestService.getPublicBundleRequests(bundleRequests.get(0).getIdPsp(),
                100, 0, bundleRequests.get(0).getCiFiscalCode(), bundleRequests.get(0).getIdBundle());

        assertEquals(bundleRequests.size(), requests.getRequestsList().size());
        assertEquals(bundleRequests.get(0).getIdBundle(), requests.getRequestsList().get(0).getIdBundle());
    }

    @Test
    void shouldAcceptRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();
        CiBundle ciBundle = getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        bundleRequestService.acceptRequest(bundleRequest.getIdPsp(), bundleRequest.getId());

        verify(ciBundleRepository, times(1)).save(ciBundle);
    }

    @Test
    void shouldThrowExceptionAlreadyAcceptedRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();
        bundleRequest.setAcceptedDate(LocalDateTime.now());

        CiBundle ciBundle = TestUtil.getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        acceptBundleRequest_ko(bundleRequest.getId(), HttpStatus.CONFLICT);
    }

    @Test
    void shouldThrowExceptionAlreadyRejectedRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();
        bundleRequest.setRejectionDate(LocalDateTime.now());

        CiBundle ciBundle = TestUtil.getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        acceptBundleRequest_ko(bundleRequest.getId(), HttpStatus.CONFLICT);
    }

    @Test
    void shouldRejectRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();
        CiBundle ciBundle = getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        bundleRequestService.rejectRequest(bundleRequest.getIdPsp(), bundleRequest.getId());

        verify(bundleRequestRepository, times(1)).delete(any());
    }

    @Test
    void shouldThrowExceptionAlreadyRejectAcceptedRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();
        bundleRequest.setAcceptedDate(LocalDateTime.now());

        CiBundle ciBundle = TestUtil.getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        rejectBundleRequest_ko(bundleRequest.getId(), HttpStatus.CONFLICT);
    }

    @Test
    void shouldThrowExceptionRejectAlreadyRejectedRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();
        bundleRequest.setRejectionDate(LocalDateTime.now());

        CiBundle ciBundle = TestUtil.getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        String idPsp = bundleRequest.getIdPsp();
        String requestId = bundleRequest.getId();
        when(bundleRequestRepository.findByIdAndIdPsp(requestId, idPsp))
                .thenReturn(Optional.of(bundleRequest));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        rejectBundleRequest_ko(bundleRequest.getId(), HttpStatus.CONFLICT);
    }

    @Test
    void createBundleRequest_ok_1() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PUBLIC);
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        BundleRequestId result = bundleRequestService.createBundleRequest(TestUtil.getMockCiFiscalCode(), TestUtil.getMockCiBundleSubscriptionRequest());
        assertNotNull(result);
    }

    @Test
    void createBundleRequest_ok_2() {
        // request with no attribute
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PUBLIC);
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setCiBundleAttributeModelList(Collections.emptyList());
        BundleRequestId result = bundleRequestService.createBundleRequest(TestUtil.getMockCiFiscalCode(), ciBundleSubscriptionRequest);
        assertNotNull(result);
    }

    @Test
    void createBundleRequest_ok_3() {
        // request with attribute null
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PUBLIC);
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setCiBundleAttributeModelList(null);
        BundleRequestId result = bundleRequestService.createBundleRequest(TestUtil.getMockCiFiscalCode(), ciBundleSubscriptionRequest);
        assertNotNull(result);
    }

    @Test
    void createBundleRequest_ok_4() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PUBLIC);
        bundle.setValidityDateTo(null);
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        BundleRequestId result = bundleRequestService.createBundleRequest(TestUtil.getMockCiFiscalCode(), TestUtil.getMockCiBundleSubscriptionRequest());
        assertNotNull(result);
    }

    @Test
    void createBundleRequest_ko_1() {
        // request for a global bundle
        Bundle bundle = TestUtil.getMockBundle();
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();

        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        AppException e = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(FISCAL_CODE, ciBundleSubscriptionRequest)
        );

        assertEquals(HttpStatus.CONFLICT, e.getHttpStatus());
    }

    @ParameterizedTest
    @ValueSource(strings = {"GLOBAL"})
    void createBundleRequest_ko_2(String bundleType) {
        // request for a private|global bundle
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.fromValue(bundleType));
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        AppException e = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(FISCAL_CODE, ciBundleSubscriptionRequest)
        );

        assertEquals(HttpStatus.CONFLICT, e.getHttpStatus());
    }

    @Test
    void createBundleRequest_ko_3() {
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        when(bundleRepository.findById(anyString())).thenReturn(Optional.empty());

        AppException e = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(FISCAL_CODE, ciBundleSubscriptionRequest)
        );

        assertEquals(HttpStatus.NOT_FOUND, e.getHttpStatus());
    }

    @Test
    void createBundleRequest_ko_4() {
        // request for a global bundle
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setValidityDateFrom(LocalDate.now().minusDays(7));
        bundle.setValidityDateTo(LocalDate.now());
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        AppException e = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(FISCAL_CODE, ciBundleSubscriptionRequest)
        );

        assertEquals(HttpStatus.BAD_REQUEST, e.getHttpStatus());
    }

    void removeBundleRequest_ko(String fiscalCode, String bundleRequestId, HttpStatus status) {
        AppException appException = assertThrows(
                AppException.class,
                () -> bundleRequestService.removeBundleRequest(fiscalCode, bundleRequestId)
        );

        assertEquals(status, appException.getHttpStatus());
    }

    void acceptBundleRequest_ko(String bundleRequestId, HttpStatus status) {
        String idPsp = TestUtil.getMockIdPsp();
        AppException appException = assertThrows(AppException.class,
                () -> bundleRequestService.acceptRequest(idPsp, bundleRequestId)
        );

        assertEquals(status, appException.getHttpStatus());
    }

    void rejectBundleRequest_ko(String bundleRequestId, HttpStatus status) {
        String idPsp = TestUtil.getMockIdPsp();
        AppException appException = assertThrows(AppException.class,
                () -> bundleRequestService.rejectRequest(idPsp, bundleRequestId)
        );

        assertEquals(status, appException.getHttpStatus());
    }

    void getBundleRequest_ko(int limit, int pageNumber, HttpStatus status) {
        String idPsp = TestUtil.getMockIdPsp();
        AppException appException = assertThrows(AppException.class,
                () -> bundleRequestService.getPublicBundleRequests(idPsp, limit, pageNumber, null, null)
        );

        assertEquals(status, appException.getHttpStatus());
    }

    private List<CiBundleAttributeModel> buildBadCIBundleAttributeModelList() {
        CiBundleAttributeModel attribute1 = CiBundleAttributeModel.builder()
                .maxPaymentAmount(100L)
                .transferCategory("taxonomy1")
                .transferCategoryRelation(TransferCategoryRelation.EQUAL)
                .build();
        CiBundleAttributeModel attribute2 = CiBundleAttributeModel.builder()
                .maxPaymentAmount(100L)
                .build();
        List<CiBundleAttributeModel> attributeModelList = new ArrayList<>();
        attributeModelList.add(attribute1);
        attributeModelList.add(attribute2);
        return attributeModelList;
    }

    private List<CiBundleAttributeModel> buildCIBundleAttributeModelList() {
        CiBundleAttributeModel attribute = CiBundleAttributeModel.builder()
                .maxPaymentAmount(100L)
                .transferCategory("taxonomy1")
                .transferCategoryRelation(TransferCategoryRelation.EQUAL)
                .build();
        return Collections.singletonList(attribute);
    }
}
