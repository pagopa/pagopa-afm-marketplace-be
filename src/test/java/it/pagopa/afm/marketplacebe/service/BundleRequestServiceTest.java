package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.request.CiBundleSubscriptionRequest;
import it.pagopa.afm.marketplacebe.model.request.CiRequests;
import it.pagopa.afm.marketplacebe.model.request.PspRequests;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import it.pagopa.afm.marketplacebe.MarketplaceBeApplication;
import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.model.request.BundleRequestId;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static it.pagopa.afm.marketplacebe.TestUtil.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@SpringBootTest
class BundleRequestServiceTest {
    @MockBean
    private BundleRepository bundleRepository;

    @MockBean
    private CiBundleRepository ciBundleRepository;

    @MockBean
    private BundleRequestRepository bundleRequestRepository;

    @Autowired
    @InjectMocks
    private BundleRequestService bundleRequestService;

    @Test
    void shouldGetRequestsByCI(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequestE());

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
    void shouldGetRequestsByCIWithoutIdPSP(){
        CiBundle ciBundle = getMockCiBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequestE());

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

    @Test
    void shouldCreateBundleRequest(){
        CiBundle ciBundle = getMockCiBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequestE());
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.PUBLIC);
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());

        // Preconditions
        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        bundleRequestService.createBundleRequest(ciBundle.getCiFiscalCode(), ciBundleSubscriptionRequest);

        verify(bundleRequestRepository, times(1)).save(Mockito.any());
    }

    @Test
    void shouldRaiseExceptionBundleTypeCreateBundleRequest(){
        CiBundle ciBundle = getMockCiBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequestE());
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.PRIVATE);

        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());

        // Preconditions
        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        AppException appException = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(ciBundle.getCiFiscalCode(), ciBundleSubscriptionRequest)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseExceptionBundleNotFoundCreateBundleRequest(){
        CiBundle ciBundle = getMockCiBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequestE());
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.PRIVATE);

        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = getMockCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setIdBundle(bundle.getId());

        // Preconditions
        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.empty());

        AppException appException = assertThrows(AppException.class,
                () -> bundleRequestService.createBundleRequest(ciBundle.getCiFiscalCode(), ciBundleSubscriptionRequest)
        );

        assertEquals(HttpStatus.NOT_FOUND, appException.getHttpStatus());
    }

    @Test
    void shouldRemoveBundleRequest(){
        BundleRequest bundleRequest = getMockBundleRequestE();

        // Preconditions
        Mockito.when(bundleRequestRepository.findById(bundleRequest.getId()))
                .thenReturn(Optional.of(bundleRequest));

        bundleRequestService.removeBundleRequest(bundleRequest.getCiFiscalCode(), bundleRequest.getId());
        verify(bundleRequestRepository, times(1)).delete(bundleRequest);
    }

    @Test
    void shouldRaiseBadRequestFiscalCodeBundleRequest(){
        BundleRequest bundleRequest = getMockBundleRequestE();
        String ciFiscalCode = "ABC";

        // Preconditions
        Mockito.when(bundleRequestRepository.findById(bundleRequest.getId()))
                .thenReturn(Optional.of(bundleRequest));

       AppException appException = assertThrows(
               AppException.class,
               () -> bundleRequestService.removeBundleRequest(ciFiscalCode, bundleRequest.getId())
       );

       assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseBadRequestNoRequest(){
        BundleRequest bundleRequest = getMockBundleRequestE();

        // Preconditions
        Mockito.when(bundleRequestRepository.findById(bundleRequest.getId()))
                .thenReturn(Optional.empty());

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleRequestService.removeBundleRequest(bundleRequest.getCiFiscalCode(), bundleRequest.getId())
        );

        assertEquals(HttpStatus.NOT_FOUND, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseBadRequestAcceptedRequest(){
        BundleRequest bundleRequest = getMockBundleRequestE();

        bundleRequest.setAcceptedDate(LocalDateTime.now().minusDays(4));
        // Preconditions
        Mockito.when(bundleRequestRepository.findById(bundleRequest.getId()))
                .thenReturn(Optional.of(bundleRequest));

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleRequestService.removeBundleRequest(bundleRequest.getCiFiscalCode(), bundleRequest.getId())
        );

        assertEquals(HttpStatus.CONFLICT, appException.getHttpStatus());
    }

    @Test
    void shouldGetRequestsByPsp(){
        List<BundleRequest> bundleRequests = List.of(getMockBundleRequestE());

        Mockito.when(bundleRequestRepository.findByIdPsp(bundleRequests.get(0).getIdPsp()))
                .thenReturn(bundleRequests);

        PspRequests requests = bundleRequestService.getRequestsByPsp(bundleRequests.get(0).getIdPsp(),
                100, 0, "", null);

        assertEquals(bundleRequests.size(), requests.getRequestsList().size());
        assertEquals(bundleRequests.get(0).getIdBundle(), requests.getRequestsList().get(0).getIdBundle());
    }

    @Test
    void shouldAcceptRequest(){
        BundleRequest bundleRequest = getMockBundleRequestE();
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
    void shouldThrowExceptionAlreadyAcceptedRequest(){
        BundleRequest bundleRequest = getMockBundleRequestE();
        bundleRequest.setAcceptedDate(LocalDateTime.now());

        CiBundle ciBundle = getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        AppException appException = assertThrows(AppException.class,
                () -> bundleRequestService.acceptRequest(bundleRequest.getIdPsp(), bundleRequest.getId())
        );

        assertEquals(HttpStatus.CONFLICT, appException.getHttpStatus());
    }

    @Test
    void shouldThrowExceptionAlreadyRejectedRequest(){
        BundleRequest bundleRequest = getMockBundleRequestE();
        bundleRequest.setRejectionDate(LocalDateTime.now());

        CiBundle ciBundle = getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        AppException appException = assertThrows(AppException.class,
                () -> bundleRequestService.acceptRequest(bundleRequest.getIdPsp(), bundleRequest.getId())
        );

        assertEquals(HttpStatus.CONFLICT, appException.getHttpStatus());
    }

    @Test
    void shouldRejectRequest(){
        BundleRequest bundleRequest = getMockBundleRequestE();
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

        verify(bundleRequestRepository, times(1)).save(Mockito.any());
    }

    @Test
    void shouldThrowExceptionAlreadyRejectAcceptedRequest(){
        BundleRequest bundleRequest = getMockBundleRequestE();
        bundleRequest.setAcceptedDate(LocalDateTime.now());

        CiBundle ciBundle = getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        AppException appException = assertThrows(AppException.class,
                () -> bundleRequestService.rejectRequest(bundleRequest.getIdPsp(), bundleRequest.getId())
        );

        assertEquals(HttpStatus.CONFLICT, appException.getHttpStatus());
    }

    @Test
    void shouldThrowExceptionRejectAlreadyRejectedRequest() {
        BundleRequest bundleRequest = getMockBundleRequestE();
        bundleRequest.setRejectionDate(LocalDateTime.now());

        CiBundle ciBundle = getMockCiBundle();
        ciBundle.setIdBundle(bundleRequest.getIdBundle());

        // Preconditions
        Mockito.when(bundleRequestRepository.findByIdAndIdPsp(bundleRequest.getId(), bundleRequest.getIdPsp()))
                .thenReturn(Optional.of(bundleRequest));
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                        bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode()
                ))
                .thenReturn(Optional.of(ciBundle));

        AppException appException = assertThrows(AppException.class,
                () -> bundleRequestService.rejectRequest(bundleRequest.getIdPsp(), bundleRequest.getId())
        );

        assertEquals(HttpStatus.CONFLICT, appException.getHttpStatus());
    }

    void createBundleRequest_ok_1() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PUBLIC);
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        BundleRequestId result = bundleRequestService.createBundleRequest(TestUtil.getMockCiFiscalCode(), TestUtil.getCiBundleSubscriptionRequest());
        assertNotNull(result);
    }

    @Test
    void createBundleRequest_ok_2() {
        // request with no attribute
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.PUBLIC);
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getCiBundleSubscriptionRequest();
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
        CiBundleSubscriptionRequest ciBundleSubscriptionRequest = TestUtil.getCiBundleSubscriptionRequest();
        ciBundleSubscriptionRequest.setCiBundleAttributeModelList(null);
        BundleRequestId result = bundleRequestService.createBundleRequest(TestUtil.getMockCiFiscalCode(), ciBundleSubscriptionRequest);
        assertNotNull(result);
    }

    @Test
    void createBundleRequest_ko_1() {
        // request for a global bundle
        Bundle bundle = TestUtil.getMockBundle();
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        createBundleRequest_ko(TestUtil.getCiBundleSubscriptionRequest(), HttpStatus.BAD_REQUEST);
    }

    @ParameterizedTest
    @ValueSource(strings = {"GLOBAL", "PRIVATE"})
    void createBundleRequest_ko_2(String bundleType) {
        // request for a private|global bundle
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setType(BundleType.fromValue(bundleType));
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        createBundleRequest_ko(TestUtil.getCiBundleSubscriptionRequest(), HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundleRequest_ko_3() {
        when(bundleRepository.findById(anyString())).thenReturn(Optional.empty());
        createBundleRequest_ko(TestUtil.getCiBundleSubscriptionRequest(), HttpStatus.NOT_FOUND);
    }



    void createBundleRequest_ko(CiBundleSubscriptionRequest ciBundleSubscriptionRequest, HttpStatus status) {
        try {
            bundleRequestService.createBundleRequest(TestUtil.getMockCiFiscalCode(), ciBundleSubscriptionRequest);
            fail();
        } catch (AppException e) {
            assertEquals(status, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
    }
}
