package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundleRequest;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.time.LocalDateTime;
import java.util.Optional;

import static it.pagopa.afm.marketplacebe.TestUtil.getMockBundleRequestEntity;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest
class BundleRequestEntityServiceTest {

    @MockBean
    BundleRequestRepository bundleRequestRepository;

    @MockBean
    ArchivedBundleRequestRepository archivedBundleRequestRepository;

    @MockBean
    CiBundleRepository ciBundleRepository;

    @Captor
    ArgumentCaptor<CiBundle> argument;

    @Autowired
    @InjectMocks
    BundleRequestService bundleRequestService;

    @Test
    void acceptRequestOk() {
        var mockBundleRequestEntity = getMockBundleRequestEntity();
        when(bundleRequestRepository.findByIdAndIdPsp(anyString(), anyString())).thenReturn(Optional.of(mockBundleRequestEntity));

        bundleRequestService.acceptRequest("123", "1");

        verify(bundleRequestRepository, times(1)).delete(any(BundleRequestEntity.class));
        verify(archivedBundleRequestRepository, times(1)).save(any(ArchivedBundleRequest.class));
        verify(ciBundleRepository, times(1)).save(argument.capture());
        assertEquals(mockBundleRequestEntity.getCiFiscalCode(), argument.getValue().getCiFiscalCode());
        assertEquals(mockBundleRequestEntity.getIdBundle(), argument.getValue().getIdBundle());
    }

    @Test
    void acceptRequestError() {
        var mockBundleRequestEntity = getMockBundleRequestEntity();
        mockBundleRequestEntity.setAcceptedDate(LocalDateTime.now());
        when(bundleRequestRepository.findByIdAndIdPsp(anyString(), anyString())).thenReturn(Optional.of(mockBundleRequestEntity));

        try {
            bundleRequestService.acceptRequest("123", "1");
        }
        catch (AppException e){
            assertEquals(HttpStatus.CONFLICT, e.getHttpStatus());
            assertEquals(AppError.REQUEST_ALREADY_ACCEPTED.title, e.getTitle());
        }
        catch (Exception e){
            fail();
        }
    }
}
