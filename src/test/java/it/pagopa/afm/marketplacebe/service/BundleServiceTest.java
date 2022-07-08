package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.bundle.*;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
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
    @Captor
    ArgumentCaptor<CiBundle> ciBundleArgumentCaptor = ArgumentCaptor.forClass(CiBundle.class);

    @Autowired
    private ModelMapper modelMapper;

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
    void shouldGetBundlesByIdPsp() {
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
    void shouldGetBundleById() {
        var bundle = getMockBundle();

        // Precondition
        Mockito.when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        BundleDetails requestedBundles = bundleService.getBundleById(bundle.getIdPsp(), bundle.getIdPsp());

        assertEquals(bundle.getId(), requestedBundles.getId());

    }

    @Test
    void shouldRaiseNotFoundException() {
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
    void shouldCreateBundle() {
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
    void shouldRaiseBadRequestWithInvalidDateBundle() {
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
    void shouldRaiseBundleNameConflict() {
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
    void shouldUpdateBundle() {
        var bundleRequest = getMockBundleModelRequest();
        Bundle bundle = getMockBundle();
        String idPsp = "test";

        // Preconditions
        // Avoid duplicate

        Mockito.when(bundleRepository.findById(bundle.getId(), new PartitionKey(idPsp)))
                .thenReturn(Optional.of(bundle));
        Mockito.when(bundleRepository.findByName(Mockito.any(), Mockito.any()))
                .thenReturn(Optional.of(bundle));

        Mockito.when(bundleRepository.save(Mockito.any()))
                .thenReturn(bundle);

        Bundle updatedBundle = bundleService.updateBundle(idPsp, bundle.getId(), bundleRequest);

        assertEquals(bundleRequest.getName(), updatedBundle.getName());
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

    @Test
    void shouldGetCIs() {
        List<CiBundle> ciBundles = List.of(getMockCiBundle());
        Bundle bundle = getMockBundle();

        Mockito.when(ciBundleRepository.findByIdBundle(bundle.getId()))
                .thenReturn(ciBundles);
        Mockito.when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        CiFiscalCodeList ciFiscalCodeList = bundleService.getCIs(bundle.getId(), bundle.getIdPsp());

        assertEquals(ciBundles.size(), ciFiscalCodeList.getCiFiscalCodeList().size());
        assertEquals(ciBundles.get(0).getCiFiscalCode(), ciFiscalCodeList.getCiFiscalCodeList().get(0));
    }

    @Test
    void shouldRaiseConflictWhenGetCIs() {
        List<CiBundle> ciBundles = List.of(getMockCiBundle());
        Bundle bundle = getMockBundle();

        Mockito.when(ciBundleRepository.findByIdBundle(bundle.getId()))
                .thenReturn(ciBundles);
        Mockito.when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
                .thenReturn(Optional.empty());

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.getCIs(bundle.getId(), bundle.getIdPsp())
        );

        assertEquals(HttpStatus.CONFLICT, appException.getHttpStatus());
    }

    @Test
    void shouldGetCIDetails() {
        Bundle bundle = getMockBundle();
        CiBundle ciBundle = getMockCiBundle();

        // Preconditions
        Mockito.when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCode(
                bundle.getId(), ciBundle.getCiFiscalCode()
        )).thenReturn(Optional.of(ciBundle));

        CiBundleDetails ciBundleDetails = bundleService.getCIDetails(
                bundle.getId(),
                bundle.getIdPsp(),
                ciBundle.getCiFiscalCode()
        );

        assertEquals(ciBundle.getAttributes().size(), ciBundleDetails.getAttributes().size());
        assertEquals(ciBundle.getAttributes().get(0).getMaxPaymentAmount(),
                ciBundleDetails.getAttributes().get(0).getMaxPaymentAmount());
    }

    @Test
    void shouldRaiseNotFoundGetCIDetails() {
        Bundle bundle = getMockBundle();
        CiBundle ciBundle = getMockCiBundle();

        // Preconditions
        Mockito.when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCode(
                bundle.getId(), ciBundle.getCiFiscalCode()
        )).thenReturn(Optional.empty());

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.getCIDetails(
                        bundle.getId(),
                        bundle.getIdPsp(),
                        ciBundle.getCiFiscalCode()
                )
        );

        assertEquals(HttpStatus.NOT_FOUND, appException.getHttpStatus());
    }

    @Test
    void shouldGetBundlesByFiscalCode(){
        CiBundle mockCIBundle = getMockCiBundle();
        List<CiBundle> ciBundles = List.of(mockCIBundle);
        Bundle bundle = getMockBundle();
        mockCIBundle.setIdBundle(bundle.getId());

        // Preconditions
        Mockito.when(ciBundleRepository.findByCiFiscalCode(mockCIBundle.getCiFiscalCode()))
                .thenReturn(ciBundles);

        Mockito.when(bundleRepository.findById(mockCIBundle.getIdBundle()))
                .thenReturn(Optional.of(bundle));

        CiBundles ciBundlesResult = bundleService.getBundlesByFiscalCode(
                mockCIBundle.getCiFiscalCode(), 100, 0
        );


        assertEquals(ciBundles.size(), ciBundlesResult.getBundleDetailsList().size());
        assertEquals(mockCIBundle.getIdBundle(),
                ciBundlesResult.getBundleDetailsList().get(0).getId());
    }

    @Test
    void shouldGetBundleByFiscalCode(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        ciBundle.setIdBundle(bundle.getId());

        // Preconditions
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                        ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));
        Mockito.when(bundleRepository.findById(bundle.getId()))
                        .thenReturn(Optional.of(bundle));

        BundleDetails bundleDetails = bundleService.getBundleByFiscalCode(ciBundle.getCiFiscalCode(), bundle.getId());

        assertEquals(bundle.getIdPsp(), bundleDetails.getIdPsp());
        assertEquals(bundle.getId(), bundleDetails.getId());
    }

    @Test
    void shouldRemoveByFiscalCode(){
        CiBundle ciBundle = getMockCiBundle();

        // Preconditions
        Mockito.when(ciBundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(ciBundle));

        bundleService.removeBundleByFiscalCode(ciBundle.getCiFiscalCode(),
                ciBundle.getIdBundle());

        verify(ciBundleRepository).delete(ciBundleArgumentCaptor.capture());

        assertEquals(ciBundle.getId(), ciBundleArgumentCaptor.getValue().getId());
    }

    @Test
    void shouldGetBundleAttributeByFiscalCode(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        ciBundle.setIdBundle(bundle.getId());

        // Preconditions
        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        BundleDetailsAttributes bundleDetailsAttributes = bundleService
                .getBundleAttributesByFiscalCode(ciBundle.getCiFiscalCode(), ciBundle.getIdBundle());

        assertEquals(ciBundle.getAttributes().get(0).getId(), bundleDetailsAttributes.getAttributes().get(0).getId());
    }

    @Test
    void shouldCreateBundleAttributesByCi(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.GLOBAL);
        ciBundle.setIdBundle(bundle.getId());

        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        Mockito.when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        BundleAttributeResponse response = bundleService.createBundleAttributesByCi(
                ciBundle.getCiFiscalCode(),
                bundle.getId(),
                getMockBundleAttributeModel()
                );

        verify(ciBundleRepository, times(1)).save(Mockito.any());
    }

    @Test
    void shouldUpdateBundleAttributesByCi(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.GLOBAL);
        ciBundle.setIdBundle(bundle.getId());

        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        Mockito.when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        bundleService.updateBundleAttributesByCi(
                ciBundle.getCiFiscalCode(),
                bundle.getId(),
                ciBundle.getAttributes().get(0).getId(),
                getMockBundleAttributeModel()
        );

        verify(ciBundleRepository, times(1)).save(Mockito.any());
    }

    @Test
    void shouldRaiseNotFoundUpdateBundleAttributesByCi(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.GLOBAL);
        ciBundle.setIdBundle(bundle.getId());

        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        Mockito.when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.updateBundleAttributesByCi(
                ciBundle.getCiFiscalCode(),
                bundle.getId(),
                UUID.randomUUID().toString(),
                getMockBundleAttributeModel())
        );

        assertEquals(HttpStatus.NOT_FOUND, appException.getHttpStatus());
    }

    @Test
    public void shouldRemoveBundleAttributesByCi(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.GLOBAL);
        ciBundle.setIdBundle(bundle.getId());

        Mockito.when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        Mockito.when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        Mockito.when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        bundleService.removeBundleAttributesByCi(
                ciBundle.getCiFiscalCode(),
                bundle.getId(),
                ciBundle.getAttributes().get(0).getId()
        );

        verify(ciBundleRepository, times(1)).delete(ciBundle);
    }
}
