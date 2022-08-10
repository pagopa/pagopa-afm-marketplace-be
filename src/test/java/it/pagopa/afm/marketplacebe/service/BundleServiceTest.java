package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.entity.*;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.CalculatorConfiguration;
import it.pagopa.afm.marketplacebe.model.bundle.*;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.task.CalculatorTaskExecutor;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
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
import java.util.*;

import static it.pagopa.afm.marketplacebe.TestUtil.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@SpringBootTest
class BundleServiceTest {
    @MockBean
    private BundleRepository bundleRepository;

    @MockBean
    private CiBundleRepository ciBundleRepository;

    @MockBean
    private BundleRequestRepository bundleRequestRepository;

    @MockBean
    private CalculatorTaskExecutor calculatorTaskExecutor;

    @Autowired
    @InjectMocks
    private BundleService bundleService;

    @Captor
    ArgumentCaptor<Bundle> bundleArgumentCaptor = ArgumentCaptor.forClass(Bundle.class);
    @Captor
    ArgumentCaptor<CiBundle> ciBundleArgumentCaptor = ArgumentCaptor.forClass(CiBundle.class);

    @Autowired
    private ModelMapper modelMapper;

    @ParameterizedTest
    @ValueSource(ints = {1, 2, 3})
    void shouldGetBundles(int bundleTypeSize) {
        var bundle = getMockBundle();
        List<Bundle> bundleList = List.of(bundle);

        // Precondition
        when(bundleRepository.getValidBundleByType(BundleType.PRIVATE.getValue())).thenReturn(bundleList);
        when(bundleRepository.getValidBundleByType(BundleType.PRIVATE.getValue(), BundleType.PUBLIC.getValue())).thenReturn(bundleList);
        when(bundleRepository.getValidBundleByType(BundleType.PRIVATE.getValue(), BundleType.PUBLIC.getValue(), BundleType.GLOBAL.getValue())).thenReturn(bundleList);

        List<BundleType> list;
        if (bundleTypeSize == 1) {
            list = List.of(BundleType.PRIVATE);
        }
        else if (bundleTypeSize == 2) {
            list = List.of(BundleType.PRIVATE, BundleType.PUBLIC);
        }
        else {
            list = List.of(BundleType.PRIVATE, BundleType.PUBLIC, BundleType.GLOBAL);
        }
        Bundles bundles = bundleService.getBundles(list);

        assertEquals(bundleList.size(), bundles.getBundleDetailsList().size());
        assertEquals(bundle.getId(), bundles.getBundleDetailsList().get(0).getId());
    }

    @Test
    void shouldGetBundlesByIdPsp() {
        var bundle = getMockBundle();
        List<Bundle> bundleList = List.of(bundle);

        List<String> requiredTypes = List.of("PRIVATE");

        // Precondition
        when(bundleRepository.findByIdPsp(bundle.getIdPsp(), new PartitionKey(bundle.getIdPsp())))
                .thenReturn(bundleList);

        Bundles bundles = bundleService.getBundlesByIdPsp(bundle.getIdPsp(), 0, 100);

        assertEquals(bundleList.size(), bundles.getBundleDetailsList().size());
        assertEquals(bundle.getId(), bundles.getBundleDetailsList().get(0).getId());
    }

    @Test
    void shouldGetBundleById() {
        var bundle = getMockBundle();

        // Precondition
        when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        PspBundleDetails requestedBundles = bundleService.getBundleById(bundle.getIdPsp(), bundle.getIdPsp());

        assertEquals(bundle.getId(), requestedBundles.getId());

    }

    @Test
    void shouldRaiseNotFoundException() {
        var bundle = getMockBundle();

        // Precondition
        when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.empty());
        String idBundle = bundle.getId();
        String idPsp = bundle.getIdPsp();
        AppException exc = assertThrows(AppException.class, () ->
                bundleService.getBundleById(idBundle, idPsp)
        );

        assertEquals(HttpStatus.NOT_FOUND, exc.getHttpStatus());
    }

    @Test
    void shouldGetBundlesRaiseException() {
        List bundles = new ArrayList();
        AppException exc = assertThrows(AppException.class, () ->
                bundleService.getBundles(bundles)
        );
        assertEquals(HttpStatus.BAD_REQUEST, exc.getHttpStatus());
    }

    @Test
    void shouldCreateBundle() {
        var bundleRequest = getMockBundleRequest();
        Bundle bundle = getMockBundle();
        String idPsp = "test_id_psp";

        when(bundleRepository.save(Mockito.any()))
                .thenReturn(bundle);

        when(bundleRepository.findByNameAndIdPsp(bundleRequest.getName(), idPsp, new PartitionKey(idPsp)))
                .thenReturn(Optional.empty());

        bundleService.createBundle(idPsp, bundleRequest);

        verify(bundleRepository).save(bundleArgumentCaptor.capture());

        Mockito.verify(bundleRepository, times(1)).save(Mockito.any());
        assertEquals(bundleRequest.getName(), bundleArgumentCaptor.getValue().getName());
    }

    @Test
    void shouldRaiseBadRequestWithInvalidDateBundle() {
        var bundleRequest = getMockBundleRequest();

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
        var bundleRequest = getMockBundleRequest();
        String idPsp = "test_id_psp";


        when(bundleRepository.findByNameAndIdPsp(Mockito.any(), Mockito.any(), Mockito.any()))
                .thenReturn(Optional.of(getMockBundle()));

        AppException appException = assertThrows(AppException.class,
                () -> bundleService.createBundle(idPsp, bundleRequest)
        );

        assertEquals(HttpStatus.CONFLICT, appException.getHttpStatus());
    }

    @Test
    void shouldUpdateBundle() {
        var bundleRequest = getMockBundleRequest();
        Bundle bundle = getMockBundle();
        String idPsp = "test";

        // Preconditions
        // Avoid duplicate

        when(bundleRepository.findById(bundle.getId(), new PartitionKey(idPsp)))
                .thenReturn(Optional.of(bundle));
        when(bundleRepository.findByNameAndIdPsp(Mockito.any(), Mockito.any(), Mockito.any()))
                .thenReturn(Optional.of(bundle));

        when(bundleRepository.save(Mockito.any()))
                .thenReturn(bundle);

        Bundle updatedBundle = bundleService.updateBundle(idPsp, bundle.getId(), bundleRequest);

        assertEquals(bundleRequest.getName(), updatedBundle.getName());
    }


    @Test
    void shouldDeleteBundle() {
        var bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(null);

        // Precondition
        when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));
        when(ciBundleRepository.findByIdBundle(anyString()))
                .thenReturn(Lists.newArrayList(getMockCiBundle()));
        when(bundleRequestRepository.findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(anyString(), anyString()))
                .thenReturn(Lists.newArrayList(getMockBundleRequestE()));

        bundleService.removeBundle(bundle.getId(), bundle.getIdPsp());

        verify(bundleRepository).save(bundleArgumentCaptor.capture());

        assertEquals(LocalDate.now(), bundleArgumentCaptor.getValue().getValidityDateTo());
        assertEquals(bundle.getId(), bundleArgumentCaptor.getValue().getId());
    }

    @Test
    void shouldGetCIs() {
        List<CiBundle> ciBundles = List.of(getMockCiBundle());
        Bundle bundle = getMockBundle();

        when(ciBundleRepository.findByIdBundle(bundle.getId()))
                .thenReturn(ciBundles);
        when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        CiFiscalCodeList ciFiscalCodeList = bundleService.getCIs(bundle.getId(), bundle.getIdPsp());

        assertEquals(ciBundles.size(), ciFiscalCodeList.getCiFiscalCodeList().size());
        assertEquals(ciBundles.get(0).getCiFiscalCode(), ciFiscalCodeList.getCiFiscalCodeList().get(0));
    }

    @Test
    void shouldRaiseConflictWhenGetCIs() {
        List<CiBundle> ciBundles = List.of(getMockCiBundle());
        Bundle bundle = getMockBundle();

        when(ciBundleRepository.findByIdBundle(bundle.getId()))
                .thenReturn(ciBundles);
        when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
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
        when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCode(
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
        when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCode(
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
        when(ciBundleRepository.findByCiFiscalCode(mockCIBundle.getCiFiscalCode()))
                .thenReturn(ciBundles);

        when(bundleRepository.findById(mockCIBundle.getIdBundle()))
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
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                        ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));
        when(bundleRepository.findById(bundle.getId()))
                        .thenReturn(Optional.of(bundle));

        BundleDetailsForCi bundleDetails = bundleService.getBundleByFiscalCode(ciBundle.getCiFiscalCode(), bundle.getId());

        assertEquals(bundle.getId(), bundleDetails.getId());
    }

    @Test
    void shouldRemoveByFiscalCode(){
        CiBundle ciBundle = getMockCiBundle();

        // Preconditions
        when(ciBundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(ciBundle));

        bundleService.removeBundleByFiscalCode(ciBundle.getCiFiscalCode(),
                ciBundle.getIdBundle());

        verify(ciBundleRepository).save(ciBundleArgumentCaptor.capture());

        assertEquals(LocalDate.now(), ciBundleArgumentCaptor.getValue().getValidityDateTo());
    }

    @Test
    void shouldGetBundleAttributeByFiscalCode(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        ciBundle.setIdBundle(bundle.getId());

        // Preconditions
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        BundleDetailsAttributes bundleDetailsAttributes = bundleService
                .getBundleAttributesByFiscalCode(ciBundle.getCiFiscalCode(), ciBundle.getIdBundle());

        assertEquals(ciBundle.getAttributes().get(0).getId(), bundleDetailsAttributes.getAttributes().get(0).getId());
    }

    @Test
    void shouldCreateBundleAttributesByCi_1(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.GLOBAL);
        ciBundle.setIdBundle(bundle.getId());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        BundleAttributeResponse response = bundleService.createBundleAttributesByCi(
                ciBundle.getCiFiscalCode(),
                bundle.getId(),
                getMockBundleAttribute()
        );

        verify(ciBundleRepository, times(1)).save(Mockito.any());
    }

    @Test
    void shouldCreateBundleAttributesByCi_2() {
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.GLOBAL);
        ciBundle.setIdBundle(bundle.getId());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.empty());

        when(ciBundleRepository.save(Mockito.any())).thenReturn(ciBundle);

        BundleAttributeResponse response = bundleService.createBundleAttributesByCi(
                ciBundle.getCiFiscalCode(),
                bundle.getId(),
                getMockBundleAttribute()
        );

        verify(ciBundleRepository, times(2)).save(Mockito.any());
    }

    @Test
    void shouldRaiseBadRequestCreateBundleAttributesByCi_1() {
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();

        when(bundleRepository.findById(bundle.getId())).thenReturn(Optional.of(bundle));

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        CiBundleAttributeModel attributes = getMockBundleAttribute();

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.createBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        attributes)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseBadRequestCreateBundleAttributesByCi_2(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.PRIVATE);

        when(bundleRepository.findById(bundle.getId())).thenReturn(Optional.of(bundle));

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        CiBundleAttributeModel attributes = getMockBundleAttribute();
        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.createBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        attributes)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseBadRequestCreateBundleAttributesByCi_3(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);

        when(bundleRepository.findById(bundle.getId())).thenReturn(Optional.of(bundle));

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        CiBundleAttributeModel attributes = getMockBundleAttribute();
        attributes.setMaxPaymentAmount(20000L);
        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.createBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        attributes)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldUpdateBundleAttributesByCi(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.GLOBAL);
        ciBundle.setIdBundle(bundle.getId());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        bundleService.updateBundleAttributesByCi(
                ciBundle.getCiFiscalCode(),
                bundle.getId(),
                ciBundle.getAttributes().get(0).getId(),
                getMockBundleAttribute()
        );

        verify(ciBundleRepository, times(1)).save(Mockito.any());
    }

    @Test
    void shouldRaiseNotFoundUpdateBundleAttributesByCi(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.GLOBAL);
        ciBundle.setIdBundle(bundle.getId());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.updateBundleAttributesByCi(
                        ciBundle.getCiFiscalCode(),
                        bundle.getId(),
                        UUID.randomUUID().toString(),
                        getMockBundleAttribute())
        );

        assertEquals(HttpStatus.NOT_FOUND, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseBadRequestUpdateBundleAttributesByCi_1(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(LocalDate.now().minusDays(1));

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        String idAttribute = UUID.randomUUID().toString();
        CiBundleAttributeModel attributes = getMockBundleAttribute();

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.updateBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        idAttribute,
                        attributes)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseBadRequestUpdateBundleAttributesByCi_2(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.PRIVATE);

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        String idAttribute = UUID.randomUUID().toString();
        CiBundleAttributeModel attributes = getMockBundleAttribute();

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.updateBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        idAttribute,
                        attributes)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseBadRequestUpdateBundleAttributesByCi_3(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        String idAttribute = UUID.randomUUID().toString();
        CiBundleAttributeModel attributes = getMockBundleAttribute();
        attributes.setMaxPaymentAmount(20000L);

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.updateBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        idAttribute,
                        attributes)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRemoveBundleAttributesByCi(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.GLOBAL);
        ciBundle.setIdBundle(bundle.getId());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        String idAttribute = ciBundle.getAttributes().get(0).getId();

        bundleService.removeBundleAttributesByCi(
                fiscalCode,
                idBundle,
                idAttribute
        );

        verify(ciBundleRepository, times(1)).delete(ciBundle);
    }

    @Test
    void shouldRaiseBadRequestRemoveBundleAttributesByCi_1(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(LocalDate.now());
        ciBundle.setIdBundle(bundle.getId());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        String idAttribute = ciBundle.getAttributes().get(0).getId();

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.removeBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        idAttribute)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseBadRequestRemoveBundleAttributesByCi_2(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.PRIVATE);
        ciBundle.setIdBundle(bundle.getId());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        String idAttribute = ciBundle.getAttributes().get(0).getId();

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.removeBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        idAttribute)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }

    @Test
    void shouldRaiseNotFoundRemoveBundleAttributesByCi_1(){
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        ciBundle.setIdBundle(bundle.getId());
        ciBundle.setAttributes(new ArrayList<>());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        String idAttribute = anyString();

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.removeBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        idAttribute)
        );

        assertEquals(HttpStatus.NOT_FOUND, appException.getHttpStatus());
    }

//    @Test
//    void shouldDeleteBundle() {
//       var bundle = getMockBundle();
//
//        // Precondition
//        Mockito.when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
//                .thenReturn(Optional.of(bundle));
//        Mockito.when(ciBundleRepository.findByIdBundle(anyString()))
//                .thenReturn(Lists.newArrayList(TestUtil.getMockCiBundle()));
//        Mockito.when(bundleRequestRepository.findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(anyString(), anyString()))
//                .thenReturn(List.of(TestUtil.getMockBundleRequestE()));
//
//        bundleService.removeBundle(bundle.getId(), bundle.getIdPsp());
//
//        verify(bundleRepository).save(bundleArgumentCaptor.capture());
//
//        assertEquals(LocalDate.now(), bundleArgumentCaptor.getValue().getValidityDateTo());
//        assertEquals(bundle.getId(), bundleArgumentCaptor.getValue().getId());
//    }

    @Test
    void createBundle_ok_1() {
        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(Collections.emptyList());

        when(bundleRepository.findByNameAndIdPsp(anyString(), anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), TestUtil.getMockBundleRequest());
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_2() {
        // same (payment method, touchpoint, type), different payment amount range and transferCategoryList
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setTransferCategoryList(new ArrayList<>());
        List<Bundle> bundles = TestUtil.getMockBundleSameConfigurationDifferentPaymentAmountRange();

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        when(bundleRepository.findByNameAndIdPsp(anyString(), anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_3() {
        // same (payment method, touchpoint, type, transferCategoryList, payment amount range), different validityDate
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now().plusDays(9));
        bundleRequest.setValidityDateTo(LocalDate.now().plusDays(12));
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        when(bundleRepository.findByNameAndIdPsp(anyString(), anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_4() {
        // same (payment method, touchpoint, type, transferCategoryList), different payment amount range and validityDate
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now().plusDays(9));
        bundleRequest.setValidityDateTo(LocalDate.now().plusDays(12));
        List<Bundle> bundles = TestUtil.getMockBundleSameConfigurationDifferentPaymentAmountRange();

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        when(bundleRepository.findByNameAndIdPsp(anyString(), anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_5() {
        // same (payment method, touchpoint, transferCategoryList, payment amount range), different type
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setType(BundleType.PUBLIC);

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(Collections.emptyList());

        when(bundleRepository.findByNameAndIdPsp(anyString(), anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ko_1() {
        // validityDateFrom > validityDateTo
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now());
        bundleRequest.setValidityDateTo(LocalDate.now().minusDays(1));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_2() {
        // same bundle configuration
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_3() {
        // same bundle configuration
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setMinPaymentAmount(20000L);
        bundleRequest.setMaxPaymentAmount(10000L);
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_4() {
        // same (payment method, touchpoint, type, transferCategoryList, payment amount range), different validityDate
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now().plusDays(8));
        bundleRequest.setValidityDateTo(LocalDate.now().plusDays(10));
        List<Bundle> bundles = TestUtil.getMockBundleWithoutValidityDateTo();

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_5() {
        // same (payment method, touchpoint, type, transferCategoryList), payment amount range overlapping
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setMaxPaymentAmount(1500L);
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_6() {
        // bundle name conflict
        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(Collections.emptyList());

        when(bundleRepository.findByNameAndIdPsp(anyString(), anyString(), any())).thenReturn(Optional.of(TestUtil.getMockBundle()));

        createBundle_ko(TestUtil.getMockBundleRequest(), HttpStatus.CONFLICT);
    }

    @Test
    void createBundle_ko_7() {
        // validityDateFrom before now
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now().minusDays(1));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_8() {
        // validityDateTo before now
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateTo(LocalDate.now());

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_9() {
        // validityDateTo before validityDateFrom
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now().plusDays(8));
        bundleRequest.setValidityDateTo(LocalDate.now().plusDays(7));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void updateBundle_ok_1() {
        Bundle bundle = TestUtil.getMockBundle();
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(Collections.emptyList());

        when(bundleRepository.findByNameAndIdNot(anyString(), anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(bundle);

        when(ciBundleRepository.findByIdBundle(anyString())).thenReturn(List.of(TestUtil.getMockCiBundle()));

        Bundle result = bundleService.updateBundle(TestUtil.getMockIdPsp(), bundle.getId(), TestUtil.getMockBundleRequest());
        assertNotNull(result);
    }

    @Test
    void updateBundle_ok_2() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setValidityDateFrom(LocalDate.now().minusDays(7));
        bundle.setValidityDateTo(LocalDate.now());

        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(Collections.emptyList());

        when(bundleRepository.findByNameAndIdNot(anyString(), anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(bundle);

        CiBundle ciBundle = getMockCiBundle();
        ciBundle.getAttributes().get(0).setMaxPaymentAmount(2000L);
        when(ciBundleRepository.findByIdBundle(anyString())).thenReturn(List.of(ciBundle));

        Bundle result = bundleService.updateBundle(TestUtil.getMockIdPsp(), bundle.getId(), TestUtil.getMockBundleRequest());
        assertNotNull(result);
    }

    @Test
    void updateBundle_ko_1() {
        // bundle name conflict
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setId("cbfbc9c6-6c0b-429e-83ca-30ef453504fa");
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        when(bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint( anyString(),
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(Collections.emptyList());

        when(bundleRepository.findByNameAndIdNot(anyString(), anyString(), any())).thenReturn(Optional.of(TestUtil.getMockBundle()));

        updateBundle_ko(bundle, HttpStatus.CONFLICT);
    }

    @Test
    void updateBundle_ko_2() {
        // validityDateTo is expired
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setId("cbfbc9c6-6c0b-429e-83ca-30ef453504fa");
        bundle.setValidityDateTo(LocalDate.now().minusDays(1));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        updateBundle_ko(bundle, HttpStatus.BAD_REQUEST);
    }

    @Test
    void updateBundle_ko_3() {
        // validityDateTo is expired
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setId("cbfbc9c6-6c0b-429e-83ca-30ef453504fa");
        bundle.setValidityDateTo(LocalDate.now());
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        updateBundle_ko(bundle, HttpStatus.BAD_REQUEST);
    }

    @Test
    void updateBundle_ko_4() {
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.empty());

        updateBundle_ko(TestUtil.getMockBundle(), HttpStatus.NOT_FOUND);
    }

    @Test
    void removeBundle_ko_1() {
        Bundle bundle = TestUtil.getMockBundle();
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        String idPsp = TestUtil.getMockIdPsp();
        String idBundle = bundle.getId();

        try {
            bundleService.removeBundle(idPsp, idBundle);
        } catch (AppException e) {
            assertEquals(HttpStatus.BAD_REQUEST, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    void getConfiguration_ok_1() {
        when(calculatorTaskExecutor.getConfiguration()).thenReturn(new CalculatorConfiguration());

        CalculatorConfiguration result = bundleService.getConfiguration();
        assertNotNull(result);
    }

    private void createBundle_ko(BundleRequest bundleRequest, HttpStatus status) {
        String idPsp = TestUtil.getMockIdPsp();

        try {
            bundleService.createBundle(idPsp, bundleRequest);
            fail();
        } catch (AppException e) {
            assertEquals(status, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
    }

    private void updateBundle_ko(Bundle bundle, HttpStatus status) {
        String idPsp = TestUtil.getMockIdPsp();
        String idBundle = bundle.getId();
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();

        try {
            bundleService.updateBundle(idPsp, idBundle, bundleRequest);
        } catch (AppException e) {
            assertEquals(status, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
    }

}
