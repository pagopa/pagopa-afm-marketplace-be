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

@SpringBootTest
public class BundleRequestServiceTest {
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
    public void shouldGetRequestsByCI(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequest());

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
    public void shouldGetRequestsByCIWithoutIdPSP(){
        CiBundle ciBundle = getMockCiBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequest());

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
    public void shouldCreateBundleRequest(){
        CiBundle ciBundle = getMockCiBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequest());
        Bundle bundle = getMockBundle();
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
    public void shouldRaiseExceptionBundleTypeCreateBundleRequest(){
        CiBundle ciBundle = getMockCiBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequest());
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
    public void shouldRaiseExceptionBundleNotFoundCreateBundleRequest(){
        CiBundle ciBundle = getMockCiBundle();
        List<BundleRequest> bundleRequest = List.of(getMockBundleRequest());
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
    public void shouldRemoveBundleRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();

        // Preconditions
        Mockito.when(bundleRequestRepository.findById(bundleRequest.getId()))
                .thenReturn(Optional.of(bundleRequest));

        bundleRequestService.removeBundleRequest(bundleRequest.getCiFiscalCode(), bundleRequest.getId());
        verify(bundleRequestRepository, times(1)).delete(bundleRequest);
    }

    @Test
    public void shouldRaiseBadRequestFiscalCodeBundleRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();
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
    public void shouldRaiseBadRequestNoRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();

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
    public void shouldRaiseBadRequestAcceptedRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();

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
    public void shouldGetRequestsByPsp(){
        List<BundleRequest> bundleRequests = List.of(getMockBundleRequest());

        Mockito.when(bundleRequestRepository.findByIdPsp(bundleRequests.get(0).getIdPsp()))
                .thenReturn(bundleRequests);

        PspRequests requests = bundleRequestService.getRequestsByPsp(bundleRequests.get(0).getIdPsp(),
                100, 0, "", null);

        assertEquals(bundleRequests.size(), requests.getRequestsList().size());
        assertEquals(bundleRequests.get(0).getIdBundle(), requests.getRequestsList().get(0).getIdBundle());
    }

    @Test
    public void shouldAcceptRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();
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
    public void shouldThrowExceptionAlreadyAcceptedRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();
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
    public void shouldThrowExceptionAlreadyRejectedRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();
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
    public void shouldRejectRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();
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
    public void shouldThrowExceptionAlreadyRejectAcceptedRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();
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
    public void shouldThrowExceptionRejectAlreadyRejectedRequest(){
        BundleRequest bundleRequest = getMockBundleRequest();
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
}
