package it.pagopa.afm.marketplacebe.service;

import static it.pagopa.afm.marketplacebe.TestUtil.getMockBundle;
import static it.pagopa.afm.marketplacebe.TestUtil.getMockBundleRequestE;
import static it.pagopa.afm.marketplacebe.TestUtil.getMockCiBundle;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import com.azure.spring.data.cosmos.core.CosmosTemplate;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.request.BundleRequestId;
import it.pagopa.afm.marketplacebe.model.request.CiBundleSubscriptionRequest;
import it.pagopa.afm.marketplacebe.model.request.CiRequests;
import it.pagopa.afm.marketplacebe.model.request.PspRequests;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;

@SpringBootTest
class BundleRequestServiceTest {
    @MockBean
    private BundleRepository bundleRepository;

    @MockBean
    private CiBundleRepository ciBundleRepository;

    @MockBean
    private BundleRequestRepository bundleRequestRepository;
    
    @MockBean 
    CosmosTemplate cosmosTemplate;

    @Autowired
    @InjectMocks
    private BundleRequestService bundleRequestService;

    @Test
    void shouldGetRequestsByCI() {
        CiBundle ciBundle = TestUtil.getMockCiBundle();
        Bundle bundle = TestUtil.getMockBundle();
        List<BundleRequestEntity> bundleRequest = List.of(TestUtil.getMockBundleRequestE());

        // Precondition
        Mockito.when(bundleRequestRepository.findByCiFiscalCodeAndIdPsp(ciBundle.getCiFiscalCode(), bundle.getIdPsp()))
                .thenReturn(bundleRequest);


        CiRequests requests = bundleRequestService.getRequestsByCI(
                ciBundle.getCiFiscalCode(),
                100,
                "",
                bundle.getIdPsp()
        );

        assertEquals(bundleRequest.size(), requests.getRequestsList().size());
        assertEquals(bundleRequest.get(0).getIdBundle(),
                requests.getRequestsList().get(0).getIdBundle());
    }

    @Test
    void shouldGetRequestsByCIWithoutIdPSP() {
        CiBundle ciBundle = TestUtil.getMockCiBundle();
        List<BundleRequestEntity> bundleRequest = List.of(TestUtil.getMockBundleRequestE());

        // Precondition
        Mockito.when(bundleRequestRepository.findByCiFiscalCode(ciBundle.getCiFiscalCode()))
                .thenReturn(bundleRequest);


        CiRequests requests = bundleRequestService.getRequestsByCI(
                ciBundle.getCiFiscalCode(),
                100,
                "",
                null
        );

        assertEquals(bundleRequest.size(), requests.getRequestsList().size());
        assertEquals(bundleRequest.get(0).getIdBundle(),
                requests.getRequestsList().get(0).getIdBundle());
    }

    @ParameterizedTest
    @ValueSource(strings = {"PUBLIC"})
    void shouldCreateBundleRequest(String bundleType) {
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.fromValue(bundleType));
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());

        // Preconditions
        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        bundleRequestService.createBundleRequest(ciBundle.getCiFiscalCode(), ciBundleSubscriptionRequest);

        verify(bundleRequestRepository, times(1)).save(Mockito.any());
    }

    @Test
    void shouldRaiseExceptionBundleTypeCreateBundleRequest_1() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.GLOBAL);

        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());

        // Preconditions
        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        createBundleRequest_ko(ciBundleSubscriptionRequest, HttpStatus.CONFLICT);
    }

    @Test
    void shouldRaiseExceptionBundleTypeCreateBundleRequest_2() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PUBLIC);

        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());
        ciBundleSubscriptionRequest.getCiBundleAttributeModelList().get(0).setMaxPaymentAmount(20000L);

        // Preconditions
        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        createBundleRequest_ko(ciBundleSubscriptionRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void shouldRaiseExceptionBundleNotFoundCreateBundleRequest() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PRIVATE);

        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());

        // Preconditions
        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.empty());

        createBundleRequest_ko(ciBundleSubscriptionRequest, HttpStatus.NOT_FOUND);
    }

    @Test
    void shouldRemoveBundleRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();

        // Preconditions
        Mockito.when(bundleRequestRepository.findById(bundleRequest.getId()))
                .thenReturn(Optional.of(bundleRequest));

        bundleRequestService.removeBundleRequest(bundleRequest.getCiFiscalCode(), bundleRequest.getId());
        verify(bundleRequestRepository, times(1)).delete(bundleRequest);
    }

    @Test
    void shouldRaiseBadRequestFiscalCodeBundleRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();
        String ciFiscalCode = "ABC";

        // Preconditions
        Mockito.when(bundleRequestRepository.findById(bundleRequest.getId()))
                .thenReturn(Optional.of(bundleRequest));

        removeBundleRequest_ko(ciFiscalCode, bundleRequest.getId(), HttpStatus.BAD_REQUEST);
    }

    @Test
    void shouldRaiseBadRequestNoRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();

        // Preconditions
        Mockito.when(bundleRequestRepository.findById(bundleRequest.getId()))
                .thenReturn(Optional.empty());

        removeBundleRequest_ko(TestUtil.getMockCiFiscalCode(), bundleRequest.getId(), HttpStatus.NOT_FOUND);
    }


    @Test
    void shouldGetRequestsByPsp_1() {
        List<BundleRequestEntity> bundleRequests = List.of(getMockBundleRequestE());

        Mockito.when(bundleRequestRepository.findByIdPspAndFiscalCodeAndIdBundle(bundleRequests.get(0).getIdPsp(), null, null, 0 ,100))
                .thenReturn(bundleRequests);

        PspRequests requests = bundleRequestService.getRequestsByPsp(bundleRequests.get(0).getIdPsp(),
                100, 0, null, null);

        assertEquals(bundleRequests.size(), requests.getRequestsList().size());
        assertEquals(bundleRequests.get(0).getIdBundle(), requests.getRequestsList().get(0).getIdBundle());
    }

    @Test
    void shouldGetRequestsByPsp_2() {
        List<BundleRequestEntity> bundleRequests = List.of(getMockBundleRequestE());

        Mockito.when(bundleRequestRepository.findByIdPspAndFiscalCodeAndIdBundle(bundleRequests.get(0).getIdPsp(), bundleRequests.get(0).getCiFiscalCode(), null, 0,100))
                .thenReturn(bundleRequests);

        PspRequests requests = bundleRequestService.getRequestsByPsp(bundleRequests.get(0).getIdPsp(),
                100, 0, bundleRequests.get(0).getCiFiscalCode(), null);

        assertEquals(bundleRequests.size(), requests.getRequestsList().size());
        assertEquals(bundleRequests.get(0).getIdBundle(), requests.getRequestsList().get(0).getIdBundle());
    }

    @Test
    void shouldGetRequestsByPsp_3() {
        List<BundleRequestEntity> bundleRequests = List.of(getMockBundleRequestE());

        Mockito.when(bundleRequestRepository.findByIdPspAndFiscalCodeAndIdBundle(bundleRequests.get(0).getIdPsp(), bundleRequests.get(0).getCiFiscalCode(),
                        bundleRequests.get(0).getIdBundle(), 0,100)).thenReturn(bundleRequests);

        PspRequests requests = bundleRequestService.getRequestsByPsp(bundleRequests.get(0).getIdPsp(),
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
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
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
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
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
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
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
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        bundleRequestService.rejectRequest(bundleRequest.getIdPsp(), bundleRequest.getId());

        verify(bundleRequestRepository, times(1)).delete(Mockito.any());
    }

    @Test
    void shouldThrowExceptionAlreadyRejectAcceptedRequest() {
        BundleRequestEntity bundleRequest = getMockBundleRequestE();
        bundleRequest.setAcceptedDate(LocalDateTime.now());

        CiBundle ciBundle = TestUtil.getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
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
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(requestId, idPsp))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
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
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        createBundleRequest_ko(TestUtil.getMockCiBundleSubscriptionRequest(), HttpStatus.CONFLICT);
    }

    @ParameterizedTest
    @ValueSource(strings = {"GLOBAL"})
    void createBundleRequest_ko_2(String bundleType) {
        // request for a private|global bundle
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.fromValue(bundleType));
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        createBundleRequest_ko(TestUtil.getMockCiBundleSubscriptionRequest(), HttpStatus.CONFLICT);
    }

    @Test
    void createBundleRequest_ko_3() {
        when(bundleRepository.findById(anyString())).thenReturn(Optional.empty());
        createBundleRequest_ko(TestUtil.getMockCiBundleSubscriptionRequest(), HttpStatus.NOT_FOUND);
    }

    @Test
    void createBundleRequest_ko_4() {
        // request for a global bundle
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setValidityDateFrom(LocalDate.now().minusDays(7));
        bundle.setValidityDateTo(LocalDate.now());
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        createBundleRequest_ko(TestUtil.getMockCiBundleSubscriptionRequest(), HttpStatus.BAD_REQUEST);
    }


    void createBundleRequest_ko(CiBundleSubscriptionRequest ciBundleSubscriptionRequest, HttpStatus status) {
        String fiscalCode = TestUtil.getMockCiFiscalCode();
        try {
            bundleRequestService.createBundleRequest(fiscalCode, ciBundleSubscriptionRequest);
            fail();
        } catch (AppException e) {
            assertEquals(status, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
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
                () -> bundleRequestService.getRequestsByPsp(idPsp, limit, pageNumber, null, null)
        );

        assertEquals(status, appException.getHttpStatus());
    }
}
