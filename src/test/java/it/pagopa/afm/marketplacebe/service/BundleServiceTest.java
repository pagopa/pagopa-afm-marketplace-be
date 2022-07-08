package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetails;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static it.pagopa.afm.marketplacebe.TestUtil.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@SpringBootTest
class BundleServiceTest {
    @MockBean
    private BundleRepository bundleRepository;

    @MockBean
    private CiBundleRepository ciBundleRepository;

    @MockBean
    private BundleRequestRepository bundleRequestRepository;

    @Autowired
    @InjectMocks
    private BundleService bundleService;

    @Captor
    ArgumentCaptor<Bundle> bundleArgumentCaptor = ArgumentCaptor.forClass(Bundle.class);


    @Test
    void shouldGetBundles() {
        var bundle = getMockBundle();
        List<Bundle> bundleList = List.of(bundle);
        List<String> requiredTypes = List.of("PRIVATE");

        // Precondition
        Mockito.when(bundleRepository.findByValidityDateToIsNullAndTypeIn(requiredTypes))
                .thenReturn(bundleList);

        Bundles bundles = bundleService.getBundles(requiredTypes.stream()
                .map(BundleType::valueOf)
                .collect(Collectors.toList()), 0, 100);

        assertEquals(bundleList.size(), bundles.getBundleDetailsList().size());
        assertEquals(bundle.getId(), bundles.getBundleDetailsList().get(0).getId());

    }

    @Test
    void shouldGetBundlesByIdPsp(){
        var bundle = getMockBundle();
        List<Bundle> bundleList = List.of(bundle);

        List<String> requiredTypes = List.of("PRIVATE");

        // Precondition
        Mockito.when(bundleRepository.findByIdPsp(bundle.getIdPsp()))
                .thenReturn(bundleList);

        Bundles bundles = bundleService.getBundlesByIdPsp(bundle.getIdPsp(), 0, 100);

        assertEquals(bundleList.size(), bundles.getBundleDetailsList().size());
        assertEquals(bundle.getId(), bundles.getBundleDetailsList().get(0).getId());

    }

    @Test
    void shouldGetBundleById(){
        var bundle = getMockBundle();

        // Precondition
        Mockito.when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        BundleDetails requestedBundles = bundleService.getBundleById(bundle.getIdPsp(), bundle.getIdPsp());

        assertEquals(bundle.getId(), requestedBundles.getId());

    }

    @Test
    void shouldRaiseNotFoundException(){
        var bundle = getMockBundle();

        // Precondition
        Mockito.when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.empty());

        AppException exc = assertThrows(AppException.class, () ->
                bundleService.getBundleById(bundle.getIdPsp(), bundle.getIdPsp())
        );

        assertEquals(HttpStatus.NOT_FOUND, exc.getHttpStatus());
    }

    @Test
    void shouldCreateBundle(){
        var bundleRequest = getMockBundleModelRequest();
        Bundle bundle = getMockBundle();
        String idPsp = "test_id_psp";

        Mockito.when(bundleRepository.save(Mockito.any()))
                .thenReturn(bundle);

        Mockito.when(bundleRepository.findByName(bundleRequest.getName(), new PartitionKey(idPsp)))
                .thenReturn(Optional.empty());

        bundleService.createBundle(idPsp, bundleRequest);

        verify(bundleRepository).save(bundleArgumentCaptor.capture());

        Mockito.verify(bundleRepository, times(1)).save(Mockito.any());
        assertEquals(bundleRequest.getName(), bundleArgumentCaptor.getValue().getName());
    }

    @Test
    void shouldRaiseBadRequestWithInvalidDateBundle(){
        var bundleRequest = getMockBundleModelRequest();

        // Setup invalid validity date to
        bundleRequest.setValidityDateTo(bundleRequest.getValidityDateFrom().minusDays(2));

        String idPsp = "test_id_psp";

        AppException appException = assertThrows(AppException.class,
                () -> bundleService.createBundle(idPsp, bundleRequest)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseBundleNameConflict(){
        var bundleRequest = getMockBundleModelRequest();
        String idPsp = "test_id_psp";


        Mockito.when(bundleRepository.findByName(Mockito.any(), Mockito.any()))
                .thenReturn(Optional.of(getMockBundle()));

        AppException appException = assertThrows(AppException.class,
                () -> bundleService.createBundle(idPsp, bundleRequest)
        );

        assertEquals(HttpStatus.CONFLICT, appException.getHttpStatus());
    }



    @Test
    void shouldDeleteBundle() {
        var bundle = getMockBundle();

        // Precondition
        Mockito.when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));
        Mockito.when(ciBundleRepository.findByIdBundle(anyString()))
                .thenReturn(Lists.newArrayList(getMockCiBundle()));
        Mockito.when(bundleRequestRepository.findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(anyString(), anyString()))
                .thenReturn(Lists.newArrayList(getMockBundleRequest()));

        bundleService.removeBundle(bundle.getId(), bundle.getIdPsp());

        verify(bundleRepository).save(bundleArgumentCaptor.capture());

        assertEquals(LocalDate.now(), bundleArgumentCaptor.getValue().getValidityDateTo());
        assertEquals(bundle.getId(), bundleArgumentCaptor.getValue().getId());
    }
}
