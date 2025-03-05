package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.config.MappingsConfiguration;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetailsAttributes;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.bundle.BundleResponse;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleDetails;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundles;
import it.pagopa.afm.marketplacebe.model.bundle.PspBundleDetails;
import it.pagopa.afm.marketplacebe.model.offer.BundleCreditorInstitutionResource;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.repository.PaymentTypeRepository;
import it.pagopa.afm.marketplacebe.repository.TouchpointRepository;
import it.pagopa.afm.marketplacebe.repository.ValidBundleRepository;
import it.pagopa.afm.marketplacebe.task.TaskManager;
import it.pagopa.afm.marketplacebe.task.ValidBundlesTaskExecutor;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;

import java.time.LocalDate;
import java.util.*;

import static it.pagopa.afm.marketplacebe.TestUtil.*;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest(classes = {BundleService.class, MappingsConfiguration.class})
class BundleServiceTest {
    @Captor
    ArgumentCaptor<Bundle> bundleArgumentCaptor = ArgumentCaptor.forClass(Bundle.class);
    @Captor
    ArgumentCaptor<CiBundle> ciBundleArgumentCaptor = ArgumentCaptor.forClass(CiBundle.class);
    @MockBean
    private BundleRepository bundleRepository;
    @MockBean
    private CiBundleRepository ciBundleRepository;
    @MockBean
    private BundleRequestRepository bundleRequestRepository;
    @MockBean
    private TouchpointRepository touchpointRepository;
    @MockBean
    private ValidBundlesTaskExecutor validBundlesTaskExecutor;
    @MockBean
    private BundleOfferRepository bundleOfferRepository;
    @MockBean
    private CosmosRepository cosmosRepository;
    @MockBean
    private PaymentTypeRepository paymentTypeRepository;
    @MockBean
    private ArchivedBundleRepository archivedBundleRepository;
    @MockBean
    private ArchivedBundleOfferRepository archivedBundleOfferRepository;
    @MockBean
    private ArchivedBundleRequestRepository archivedBundleRequestRepository;
    @MockBean
    private ArchivedCiBundleRepository archivedCiBundleRepository;
    @MockBean
    private ValidBundleRepository validBundleRepository;

    @MockBean
    private TaskManager taskManager;
    @Autowired
    private BundleService bundleService;


    @ParameterizedTest
    @ValueSource(ints = {1, 2, 3})
    void shouldGetBundles(int bundleTypeSize) {
        var bundle = getMockBundle();
        List<Bundle> bundleList = List.of(bundle);

        // Precondition
        when(cosmosRepository.getBundlesByNameAndTypeAndValidityDateFromAndExpireAt(eq(null), anyList(), eq(null), eq(null), anyInt(), anyInt()))
                .thenReturn(bundleList);
        when(cosmosRepository.getTotalItemsFindByNameAndTypeAndValidityDateFromAndExpireAt(eq(null), anyList(), eq(null), eq(null)))
                .thenReturn((long) bundleList.size());

        List<BundleType> list;
        if (bundleTypeSize == 1) {
            list = List.of(BundleType.PRIVATE);
        } else if (bundleTypeSize == 2) {
            list = List.of(BundleType.PRIVATE, BundleType.PUBLIC);
        } else {
            list = List.of(BundleType.PRIVATE, BundleType.PUBLIC, BundleType.GLOBAL);
        }

        Bundles bundles = assertDoesNotThrow(() -> bundleService.getBundles(list, null, null, null, 50, 0));

        assertEquals(bundleList.size(), bundles.getBundleDetailsList().size());
        assertEquals(bundle.getId(), bundles.getBundleDetailsList().get(0).getId());
        assertEquals(bundleList.size(), bundles.getPageInfo().getTotalItems());
    }

    @Test
    void shouldGetBundlesByName() {
        var bundle = getMockBundle();
        List<Bundle> bundleList = List.of(bundle);

        // Precondition
        when(cosmosRepository.getBundlesByNameAndTypeAndValidityDateFromAndExpireAt(anyString(), anyList(), eq(null), eq(null), anyInt(), anyInt()))
                .thenReturn(bundleList);
        when(cosmosRepository.getTotalItemsFindByNameAndTypeAndValidityDateFromAndExpireAt(anyString(), anyList(), eq(null), eq(null)))
                .thenReturn((long) bundleList.size());

        List<BundleType> bundleTypeList = new ArrayList<>();
        bundleTypeList.add(BundleType.GLOBAL);
        String nameParams = "mockName";
        Bundles bundles = bundleService.getBundles(bundleTypeList, nameParams, null, null, 50, 0);

        assertEquals(bundleList.size(), bundles.getBundleDetailsList().size());
        assertEquals(bundle.getId(), bundles.getBundleDetailsList().get(0).getId());
        assertEquals(bundleList.size(), bundles.getPageInfo().getTotalItems());
    }

    @Test
    void shouldGetBundlesByValidDate() {
        var bundle = getMockBundle();
        List<Bundle> bundleList = List.of(bundle);

        // Precondition
        when(cosmosRepository.getBundlesByNameAndTypeAndValidityDateFromAndExpireAt(eq(null), anyList(), any(), any(), anyInt(), anyInt()))
                .thenReturn(bundleList);
        when(cosmosRepository.getTotalItemsFindByNameAndTypeAndValidityDateFromAndExpireAt(eq(null), anyList(), any(), any()))
                .thenReturn((long) bundleList.size());

        List<BundleType> bundleTypeList = new ArrayList<>();
        bundleTypeList.add(BundleType.GLOBAL);
        Bundles bundles = bundleService.getBundles(bundleTypeList, null, LocalDate.now(), LocalDate.now(), 50, 0);

        assertEquals(bundleList.size(), bundles.getBundleDetailsList().size());
        assertEquals(bundle.getId(), bundles.getBundleDetailsList().get(0).getId());
        assertEquals(bundleList.size(), bundles.getPageInfo().getTotalItems());
    }

    @Test
    void shouldGetBundlesByIdPsp() {
        var bundle = getMockBundle();
        List<Bundle> bundleList = List.of(bundle);

        // Precondition
        when(cosmosRepository.getBundlesByNameAndType(anyString(), any(), anyList(), any(), any(), any(), any(), any(), any(), any(), anyInt(), anyInt())).thenReturn(bundleList);

        List<BundleType> bundleParams = new ArrayList<>();
        bundleParams.add(BundleType.GLOBAL);
        Bundles bundles = bundleService.getBundlesByIdPsp(bundle.getIdPsp(), bundleParams, null, Sort.Direction.ASC, null, null, null, null, null, null, 0, 50);

        assertEquals(bundleList.size(), bundles.getBundleDetailsList().size());
        assertEquals(bundle.getId(), bundles.getBundleDetailsList().get(0).getId());
    }

    @Test
    void shouldGetBundlesByIdPspAndName() {
        var bundle = getMockBundle();
        List<Bundle> bundleList = List.of(bundle);

        // Precondition
        when(cosmosRepository.getBundlesByNameAndType(anyString(), anyString(), anyList(), any(), any(), any(), any(), any(), any(), any(), anyInt(), anyInt())).thenReturn(bundleList);

        List<BundleType> bundleParams = new ArrayList<>();
        bundleParams.add(BundleType.GLOBAL);
        Bundles bundles = bundleService.getBundlesByIdPsp(bundle.getIdPsp(), bundleParams, "mockName", Sort.Direction.ASC, null, null, null, null, null, null, 0, 50);

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
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.empty());
        String idBundle = bundle.getId();
        String idPsp = bundle.getIdPsp();
        AppException exc = assertThrows(AppException.class, () ->
                bundleService.getBundleById(idBundle, idPsp)
        );

        assertEquals(HttpStatus.NOT_FOUND, exc.getHttpStatus());
    }

    @Test
    void shouldCreateBundle() {
        var bundleRequest = getMockBundleRequest();
        Bundle bundle = getMockBundle();
        String idPsp = "test_id_psp";

        when(bundleRepository.save(Mockito.any()))
                .thenReturn(bundle);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));


        when(paymentTypeRepository.findByName(bundle.getPaymentType())).thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        bundleService.createBundle(idPsp, bundleRequest);

        verify(bundleRepository).save(bundleArgumentCaptor.capture());

        Mockito.verify(bundleRepository, times(1)).save(Mockito.any());
        assertEquals(bundleRequest.getName(), bundleArgumentCaptor.getValue().getName());
        assertEquals(bundleRequest.getPspBusinessName(), bundleArgumentCaptor.getValue().getPspBusinessName());
    }

    @Test
    void shouldCreateBundleWithPaymentTypeNull() {
        var bundleRequest = TestUtil.getMockBundleRequestWithPaymentTypeNull();
        Bundle bundle = getMockBundle();
        String idPsp = "test_id_psp";

        when(bundleRepository.save(Mockito.any()))
                .thenReturn(bundle);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundle.getPaymentType())).thenReturn(Optional.of(TestUtil.getMockPaymentType()));

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
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        String idPsp = "test_id_psp";

        AppException appException = assertThrows(AppException.class,
                () -> bundleService.createBundle(idPsp, bundleRequest)
        );

        assertEquals(HttpStatus.BAD_REQUEST, appException.getHttpStatus());
    }


    @Test
    void shouldUpdateBundle() {
        var bundleRequest = getMockBundleRequest();
        Bundle bundle = getMockBundle();
        String idPsp = "test";

        // Preconditions
        // Avoid duplicate


        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(bundleRepository.findById(bundle.getId(), new PartitionKey(idPsp)))
                .thenReturn(Optional.of(bundle));
        when(paymentTypeRepository.findByName(bundle.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        when(bundleRepository.save(Mockito.any()))
                .thenReturn(bundle);


        Bundle updatedBundle = bundleService.updateBundle(idPsp, bundle.getId(), bundleRequest, false);

        assertEquals(bundleRequest.getName(), updatedBundle.getName());
    }

    @Test
    void shouldUpdateBundleWithPaymentTypeNull() {
        var bundleRequest = TestUtil.getMockBundleRequestWithPaymentTypeNull();
        Bundle bundle = getMockBundle();
        String idPsp = "test";

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(bundleRepository.findById(bundle.getId(), new PartitionKey(idPsp)))
                .thenReturn(Optional.of(bundle));
        when(paymentTypeRepository.findByName(bundle.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        when(bundleRepository.save(Mockito.any()))
                .thenReturn(bundle);

        Bundle updatedBundle = bundleService.updateBundle(idPsp, bundle.getId(), bundleRequest, false);

        assertEquals(bundleRequest.getName(), updatedBundle.getName());
    }


    @Test
    void shouldDeleteBundle() {
        var bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(null);

        ArrayList<BundleRequestEntity> requests = Lists.newArrayList(getMockBundleRequestE());
        ArrayList<BundleOffer> offers = Lists.newArrayList(getMockBundleOffer());

        // Precondition
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));
        when(ciBundleRepository.findByIdBundle(anyString())).thenReturn(Lists.newArrayList(getMockCiBundle()));
        when(bundleRequestRepository.findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(anyString(), anyString()))
                .thenReturn(requests);
        when(bundleOfferRepository.findByIdPspAndIdBundleAndAcceptedDateIsNullAndRejectionDateIsNull(anyString(), anyString()))
                .thenReturn(offers);

        assertDoesNotThrow(() -> bundleService.removeBundle(bundle.getId(), bundle.getIdPsp()));

        verify(bundleRepository).save(bundleArgumentCaptor.capture());
        verify(bundleRequestRepository).deleteAll(requests);
        verify(bundleOfferRepository).deleteAll(offers);

        assertEquals(LocalDate.now(), bundleArgumentCaptor.getValue().getValidityDateTo());
        assertEquals(bundle.getId(), bundleArgumentCaptor.getValue().getId());
    }

    @Test
    void removeBundle_ko_1() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setValidityDateTo(LocalDate.now());

        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        String bundleId = bundle.getId();
        String idPsp = bundle.getIdPsp();
        AppException e = assertThrows(AppException.class, () -> bundleService.removeBundle(bundleId, idPsp));

        assertEquals(HttpStatus.BAD_REQUEST, e.getHttpStatus());
    }

    @Test
    void shouldGetCIs() {
        List<CiBundle> ciBundles = List.of(getMockCiBundle());
        Bundle bundle = getMockBundle();

        when(ciBundleRepository.findByIdBundleAndCiFiscalCode(bundle.getId(), null, 0, 100))
                .thenReturn(ciBundles);
        when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        BundleCreditorInstitutionResource ciFiscalCodeList = bundleService.getCIs(bundle.getId(), bundle.getIdPsp(), null, 100, 0);

        assertEquals(ciBundles.size(), ciFiscalCodeList.getCiBundleDetails().size());
        assertEquals(ciBundles.get(0).getCiFiscalCode(), ciFiscalCodeList.getCiBundleDetails().get(0).getCiTaxCode());
    }

    @Test
    void shouldRaiseConflictWhenGetCIs() {
        List<CiBundle> ciBundles = List.of(getMockCiBundle());
        Bundle bundle = getMockBundle();

        when(ciBundleRepository.findByIdBundleAndCiFiscalCode(bundle.getId(), null, 0, 100))
                .thenReturn(ciBundles);
        when(bundleRepository.findById(Mockito.anyString(), Mockito.any(PartitionKey.class)))
                .thenReturn(Optional.empty());

        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.getCIs(bundle.getId(), bundle.getIdPsp(), null, 100, 0)
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
    void shouldGetBundlesByFiscalCode() {
        CiBundle mockCIBundle = getMockCiBundle();
        List<CiBundle> ciBundles = List.of(mockCIBundle);
        Bundle bundle = getMockBundle();
        mockCIBundle.setIdBundle(bundle.getId());

        // Preconditions
        when(ciBundleRepository.findByCiFiscalCodeAndTypeAndIdBundles(anyString(), eq(null), eq(null), anyInt(), anyInt()))
                .thenReturn(ciBundles);
        when(ciBundleRepository.getTotalItemsFindByCiFiscalCodeAndTypeAndIdBundles(anyString(), eq(null), eq(null)))
                .thenReturn(1);

        CiBundles ciBundlesResult = bundleService.getBundlesByFiscalCode(
                mockCIBundle.getCiFiscalCode(), null, null, null, 100, 0);


        assertEquals(ciBundles.size(), ciBundlesResult.getBundleDetailsList().size());
        assertEquals(mockCIBundle.getIdBundle(),
                ciBundlesResult.getBundleDetailsList().get(0).getIdBundle());
        assertEquals(0, ciBundlesResult.getPageInfo().getPage());
        assertEquals(1, ciBundlesResult.getPageInfo().getTotalItems());
        assertEquals(1, ciBundlesResult.getPageInfo().getTotalPages());
    }

    @Test
    void shouldGetBundlesByFiscalCodeTypeFilter() {
        CiBundle mockCIBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        mockCIBundle.setIdBundle(bundle.getId());

        // Preconditions
        when(ciBundleRepository.findByCiFiscalCodeAndTypeAndIdBundles(mockCIBundle.getCiFiscalCode(), "GLOBAL", null, 0, 100))
                .thenReturn(new ArrayList<>());
        when(ciBundleRepository.getTotalItemsFindByCiFiscalCodeAndTypeAndIdBundles(mockCIBundle.getCiFiscalCode(), "GLOBAL", null))
                .thenReturn(0);

        CiBundles ciBundlesResult = bundleService.getBundlesByFiscalCode(
                mockCIBundle.getCiFiscalCode(), BundleType.GLOBAL, null, null, 100, 0);

        assertEquals(0, ciBundlesResult.getBundleDetailsList().size());
        assertEquals(0, ciBundlesResult.getPageInfo().getPage());
        assertEquals(0, ciBundlesResult.getPageInfo().getTotalItems());
        assertEquals(0, ciBundlesResult.getPageInfo().getTotalPages());
    }

    @Test
    void shouldGetBundlesByFiscalCodeAndPspBusinessNameFilter() {
        CiBundle mockCIBundle = getMockCiBundle();
        List<CiBundle> ciBundles = List.of(mockCIBundle);
        Bundle bundle = getMockBundle();
        mockCIBundle.setIdBundle(bundle.getId());

        // Preconditions
        when(ciBundleRepository.findByCiFiscalCodeAndTypeAndIdBundles(anyString(), eq(null), anyList(), anyInt(), anyInt()))
                .thenReturn(ciBundles);
        when(ciBundleRepository.getTotalItemsFindByCiFiscalCodeAndTypeAndIdBundles(anyString(), eq(null), anyList()))
                .thenReturn(1);
        when(cosmosRepository.getBundlesByNameAndPSPBusinessName(bundle.getPspBusinessName(), null, null))
                .thenReturn(List.of(bundle));

        CiBundles ciBundlesResult = bundleService.getBundlesByFiscalCode(
                mockCIBundle.getCiFiscalCode(),
                null,
                null,
                bundle.getPspBusinessName(),
                100,
                0
        );


        assertEquals(ciBundles.size(), ciBundlesResult.getBundleDetailsList().size());
        assertEquals(mockCIBundle.getIdBundle(),
                ciBundlesResult.getBundleDetailsList().get(0).getIdBundle());
        assertEquals(mockCIBundle.getId(),
                ciBundlesResult.getBundleDetailsList().get(0).getIdCIBundle());
        assertEquals(0, ciBundlesResult.getPageInfo().getPage());
        assertEquals(1, ciBundlesResult.getPageInfo().getTotalItems());
        assertEquals(1, ciBundlesResult.getPageInfo().getTotalPages());
    }

    @Test
    void shouldGetBundlesByFiscalCodeAndTypeAndPspBusinessNameFilters() {
        CiBundle mockCIBundle = getMockCiBundle();
        List<CiBundle> ciBundles = List.of(mockCIBundle);
        Bundle bundle = getMockBundle();
        mockCIBundle.setIdBundle(bundle.getId());

        // Preconditions
        when(ciBundleRepository.findByCiFiscalCodeAndTypeAndIdBundles(anyString(), anyString(), anyList(), anyInt(), anyInt()))
                .thenReturn(ciBundles);
        when(ciBundleRepository.getTotalItemsFindByCiFiscalCodeAndTypeAndIdBundles(anyString(), anyString(), anyList()))
                .thenReturn(1);
        when(cosmosRepository.getBundlesByNameAndPSPBusinessName(bundle.getPspBusinessName(), null, "PRIVATE"))
                .thenReturn(List.of(bundle));

        CiBundles ciBundlesResult = bundleService.getBundlesByFiscalCode(
                mockCIBundle.getCiFiscalCode(),
                BundleType.PRIVATE,
                null,
                bundle.getPspBusinessName(),
                100,
                0
        );


        assertEquals(ciBundles.size(), ciBundlesResult.getBundleDetailsList().size());
        assertEquals(mockCIBundle.getIdBundle(),
                ciBundlesResult.getBundleDetailsList().get(0).getIdBundle());
        assertEquals(mockCIBundle.getId(),
                ciBundlesResult.getBundleDetailsList().get(0).getIdCIBundle());
        assertEquals(0, ciBundlesResult.getPageInfo().getPage());
        assertEquals(1, ciBundlesResult.getPageInfo().getTotalItems());
        assertEquals(1, ciBundlesResult.getPageInfo().getTotalPages());
    }

    @Test
    void shouldGetBundlesByFiscalCodeAndTypeAndBundleNameFilters() {
        CiBundle mockCIBundle = getMockCiBundle();
        List<CiBundle> ciBundles = List.of(mockCIBundle);
        Bundle bundle = getMockBundle();
        mockCIBundle.setIdBundle(bundle.getId());

        // Preconditions
        when(ciBundleRepository.findByCiFiscalCodeAndTypeAndIdBundles(anyString(), anyString(), anyList(), anyInt(), anyInt()))
                .thenReturn(ciBundles);
        when(ciBundleRepository.getTotalItemsFindByCiFiscalCodeAndTypeAndIdBundles(anyString(), anyString(), anyList()))
                .thenReturn(1);
        when(cosmosRepository.getBundlesByNameAndPSPBusinessName(null, bundle.getName(), "PRIVATE"))
                .thenReturn(List.of(bundle));

        CiBundles ciBundlesResult = bundleService.getBundlesByFiscalCode(
                mockCIBundle.getCiFiscalCode(),
                BundleType.PRIVATE,
                bundle.getName(),
                null,
                100,
                0
        );


        assertEquals(ciBundles.size(), ciBundlesResult.getBundleDetailsList().size());
        assertEquals(mockCIBundle.getIdBundle(),
                ciBundlesResult.getBundleDetailsList().get(0).getIdBundle());
        assertEquals(mockCIBundle.getId(),
                ciBundlesResult.getBundleDetailsList().get(0).getIdCIBundle());
        assertEquals(0, ciBundlesResult.getPageInfo().getPage());
        assertEquals(1, ciBundlesResult.getPageInfo().getTotalItems());
        assertEquals(1, ciBundlesResult.getPageInfo().getTotalPages());
    }

    @Test
    void shouldGetBundleByFiscalCode() {
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        ciBundle.setIdBundle(bundle.getId());

        // Preconditions
        when(ciBundleRepository.findByIdBundleAndCiFiscalCode(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));
        when(bundleRepository.findById(bundle.getId())).thenReturn(Optional.of(bundle));

        CiBundleDetails bundleDetails = bundleService.getBundleByFiscalCode(ciBundle.getCiFiscalCode(), bundle.getId());

        assertEquals(bundle.getId(), bundleDetails.getIdBundle());
    }

    @Test
    void shouldRemoveByFiscalCode() {
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
    void shouldGetBundleAttributeByFiscalCode() {
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
    void shouldCreateBundleAttributesByCi_1() {
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        // Valid bundle
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.PUBLIC);
        ciBundle.setIdBundle(bundle.getId());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        when(ciBundleRepository.save(Mockito.any()))
                .thenReturn(ciBundle);

        bundleService.createBundleAttributesByCi(
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
        bundle.setType(BundleType.PUBLIC);
        ciBundle.setIdBundle(bundle.getId());

        when(bundleRepository.findById(bundle.getId()))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.empty());

        when(ciBundleRepository.save(Mockito.any())).thenReturn(ciBundle);

        bundleService.createBundleAttributesByCi(
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
    void shouldRaiseBadRequestCreateBundleAttributesByCi_2() {
        CiBundle ciBundle = getMockCiBundle();
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        bundle.setType(BundleType.GLOBAL);

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
    void shouldRaiseBadRequestCreateBundleAttributesByCi_3() {
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
    void shouldRaiseConflictCreateBundleAttributesByCi_1() {
        CiBundle ciBundle = getMockCiBundleNoAttributes();
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.PUBLIC);

        when(bundleRepository.findById(bundle.getId())).thenReturn(Optional.of(bundle));

        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(),
                ciBundle.getCiFiscalCode())).thenReturn(Optional.of(ciBundle));

        String fiscalCode = ciBundle.getCiFiscalCode();
        String idBundle = bundle.getId();
        CiBundleAttributeModel attributes = getMockBundleAttribute();
        attributes.setMaxPaymentAmount(50L);
        AppException appException = assertThrows(
                AppException.class,
                () -> bundleService.createBundleAttributesByCi(
                        fiscalCode,
                        idBundle,
                        attributes)
        );

        assertEquals(HttpStatus.CONFLICT, appException.getHttpStatus());
    }

    @Test
    void shouldUpdateBundleAttributesByCi() {
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
    void shouldRaiseNotFoundUpdateBundleAttributesByCi() {
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
    void shouldRaiseBadRequestUpdateBundleAttributesByCi_1() {
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
    void shouldRaiseBadRequestUpdateBundleAttributesByCi_2() {
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
    void shouldRaiseBadRequestUpdateBundleAttributesByCi_3() {
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
    void shouldRemoveBundleAttributesByCi() {
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
    void shouldRaiseBadRequestRemoveBundleAttributesByCi_1() {
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
    void shouldRaiseBadRequestRemoveBundleAttributesByCi_2() {
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
    void shouldRaiseNotFoundRemoveBundleAttributesByCi_1() {
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
        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(Collections.emptyList());

        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(paymentTypeRepository.findByName(anyString()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), TestUtil.getMockBundleRequest());
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_2() {
        // same (payment method, touchpoint, type), different payment amount range and transferCategoryList
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setTransferCategoryList(new ArrayList<>());
        List<Bundle> bundles = TestUtil.getMockBundleSameConfigurationDifferentPaymentAmountRange();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

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

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

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

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_5() {
        // same (payment method, touchpoint, transferCategoryList, payment amount range), different type
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setType(BundleType.PUBLIC);

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(Collections.emptyList());

        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        when(touchpointRepository.findByName(bundleRequest.getTouchpoint())).thenReturn(Optional.of(
                TestUtil.getMockTouchpoint(bundleRequest.getTouchpoint())));
        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_6() {
        // same (payment method, touchpoint, transferCategoryList, payment amount range), different type
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setPaymentType(null);
        bundleRequest.setTouchpoint(null);
        bundleRequest.setType(BundleType.PUBLIC);

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(Collections.emptyList());

        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());
        when(paymentTypeRepository.findByName("ANY"))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_7() {
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setPaymentType("CP");
        bundleRequest.setDigitalStamp(true);
        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(Collections.emptyList());

        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_8() {
        // create two bundle with same idPsp and same Name
        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(Collections.emptyList());

        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(paymentTypeRepository.findByName(anyString()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result1 = bundleService.createBundle(TestUtil.getMockIdPsp(), TestUtil.getMockBundleRequest());
        assertNotNull(result1);
        BundleResponse result2 = bundleService.createBundle(TestUtil.getMockIdPsp(), TestUtil.getMockBundleRequest());
        assertNotNull(result2);
    }

    @Test
    void createBundle_ok_9() {
        // same (payment method, touchpoint, payment amount range), different transferCategoryList
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setTransferCategoryList(Arrays.asList("taxonomy3", "taxonomy4"));
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_10() {
        // same (payment method, touchpoint, payment amount range), transferCategoryList target null
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();
        bundles.get(0).setTransferCategoryList(null);

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_11() {
        // same (payment method, touchpoint, transferCategoryList, payment amount range), idChannel ONUS case
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setIdChannel("idChannel_ONUS");
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_12() {
        // same (payment method, touchpoint, transferCategoryList, payment amount range), idChannel ONUS case and transferCategoryList null
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setIdChannel("idChannel_ONUS");
        bundleRequest.setTransferCategoryList(null);
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_13() {
        // same (payment method, touchpoint, transferCategoryList, payment amount range, idChannel), and bundle type is PRIVATE.
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setType(BundleType.PRIVATE);
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ko_1() {
        // validityDateFrom > validityDateTo
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now());
        bundleRequest.setValidityDateTo(LocalDate.now().minusDays(1));

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_2() {
        // same bundle configuration
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_3() {
        // same bundle configuration
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setMinPaymentAmount(20000L);
        bundleRequest.setMaxPaymentAmount(10000L);
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_4() {
        // same (payment method, touchpoint, type, transferCategoryList, payment amount range), different validityDate
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now().plusDays(8));
        bundleRequest.setValidityDateTo(LocalDate.now().plusDays(10));
        List<Bundle> bundles = TestUtil.getMockBundleWithoutValidityDateTo();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_5() {
        // same (payment method, touchpoint, type, transferCategoryList), payment amount range overlapping
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setMaxPaymentAmount(1500L);
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }


    @Test
    void createBundle_ko_7() {
        // validityDateFrom before now
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now().minusDays(1));

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_8() {
        // validityDateTo before now
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateTo(LocalDate.now());

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_9() {
        // validityDateTo before validityDateFrom
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now().plusDays(8));
        bundleRequest.setValidityDateTo(LocalDate.now().plusDays(7));

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_10() {
        // Same bundle configuration with a different idChannel, but it does not end with _ONUS.
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setIdChannel("idChannel_NOT-ONUS");
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_11() {
        // Same bundle configuration for two bundle (1 ONUS, 1 NOT ONUS) with different normalized idChannel.
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();
        bundles.get(0).setIdChannel("idChannel1_ONUS");
        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_12() {
        // Same bundle configuration for two bundle with same idChannel ONUS.
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setIdChannel("idChannel_ONUS");
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();
        bundles.get(0).setIdChannel("idChannel_ONUS");
        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(bundles);

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        when(paymentTypeRepository.findByName(bundleRequest.getPaymentType()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void updateBundle_ok_1() {
        Bundle bundle = TestUtil.getMockBundle();
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(Collections.emptyList());

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(bundleRepository.save(any(Bundle.class))).thenReturn(bundle);

        when(ciBundleRepository.findByIdBundle(anyString())).thenReturn(List.of(TestUtil.getMockCiBundle()));
        when(paymentTypeRepository.findByName(bundle.getPaymentType())).thenReturn(Optional.of(TestUtil.getMockPaymentType()));


        Bundle result = bundleService.updateBundle(TestUtil.getMockIdPsp(), bundle.getId(), TestUtil.getMockBundleRequest(), false);
        assertNotNull(result);
        assertEquals(result.getPspBusinessName(), TestUtil.getMockBundleRequest().getPspBusinessName());
    }

    @Test
    void updateBundle_ok_2() {
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setValidityDateFrom(LocalDate.now().minusDays(7));
        bundle.setValidityDateTo(LocalDate.now());

        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(Collections.emptyList());


        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(bundleRepository.save(any(Bundle.class))).thenReturn(bundle);

        CiBundle ciBundle = getMockCiBundle();
        ciBundle.getAttributes().get(0).setMaxPaymentAmount(2000L);
        when(ciBundleRepository.findByIdBundle(anyString())).thenReturn(List.of(ciBundle));

        when(paymentTypeRepository.findByName(bundle.getPaymentType())).thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        Bundle result = bundleService.updateBundle(TestUtil.getMockIdPsp(), bundle.getId(), TestUtil.getMockBundleRequest(), false);
        assertNotNull(result);
    }

    @Test
    void updateBundle_ok_3() {
        BundleRequest bundleRequest = getMockBundleRequest();
        bundleRequest.setPaymentType("CP");
        bundleRequest.setDigitalStamp(true);
        Bundle bundle = TestUtil.getMockBundle();
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(Collections.emptyList());

        when(bundleRepository.save(any(Bundle.class))).thenReturn(bundle);

        when(ciBundleRepository.findByIdBundle(anyString())).thenReturn(List.of(TestUtil.getMockCiBundle()));

        when(touchpointRepository.findByName(bundleRequest.getTouchpoint())).thenReturn(Optional.of(
                TestUtil.getMockTouchpoint(bundleRequest.getTouchpoint())));

        when(paymentTypeRepository.findByName(bundle.getPaymentType())).thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        Bundle result = bundleService.updateBundle(TestUtil.getMockIdPsp(), bundle.getId(), bundleRequest, false);
        assertNotNull(result);
    }


    @Test
    void updateBundle_ko_2() {
        // validityDateTo is expired
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setId("cbfbc9c6-6c0b-429e-83ca-30ef453504fa");
        bundle.setValidityDateTo(LocalDate.now().minusDays(1));

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        updateBundle_ko(bundle, HttpStatus.BAD_REQUEST);
    }

    @Test
    void updateBundle_ko_3() {
        // validityDateTo is expired
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setId("cbfbc9c6-6c0b-429e-83ca-30ef453504fa");
        bundle.setValidityDateTo(LocalDate.now());

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));
        when(paymentTypeRepository.findByName(bundle.getPaymentType())).thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        updateBundle_ko(bundle, HttpStatus.BAD_REQUEST);
    }

    @Test
    void updateBundle_ko_4() {
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.empty());

        updateBundle_ko(TestUtil.getMockBundle(), HttpStatus.NOT_FOUND);
    }

    @Test
    void updateBundle_ko_5() {
        //update overlaps configuration of an existing bundle
        Bundle bundle = TestUtil.getMockBundle();
        bundle.setId("cbfbc9c6-6c0b-429e-83ca-30ef453504fa");
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));
        when(paymentTypeRepository.findByName(bundle.getPaymentType())).thenReturn(Optional.of(TestUtil.getMockPaymentType()));
        //setting a different id to trigger overlapping
        bundle.setId("jhk14kl8-4lb6-d49s-s5f9-jk23bnrtk4l3");
        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(), any(BundleType.class), anyString(), anyString())).thenReturn(List.of(bundle));

        updateBundle_ko(TestUtil.getMockBundle(), HttpStatus.BAD_REQUEST);
    }

    @Test
    void getConfiguration_ok() {
        // only to verify all tasks are called
        when(bundleRepository.findByValidityDateToBefore(any())).thenReturn(new ArrayList<>());
        when(bundleOfferRepository.findByValidityDateToBefore(any())).thenReturn(new ArrayList<>());
        when(bundleRequestRepository.findByValidityDateToBefore(any())).thenReturn(new ArrayList<>());
        when(ciBundleRepository.findByValidityDateToBefore(any())).thenReturn(new ArrayList<>());
        when(bundleRepository.findAll()).thenReturn(new ArrayList<>());
        when(ciBundleRepository.findAll()).thenReturn(new ArrayList<>());

        assertDoesNotThrow(() -> bundleService.getConfiguration());
    }

    @Test
    void createBundleByList_ok() {
        when(bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(anyString(),
                any(BundleType.class), anyString(), anyString())).thenReturn(Collections.emptyList());

        when(bundleRepository.saveAll(any())).thenReturn(TestUtil.getMockBundleList());
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        when(paymentTypeRepository.findByName(anyString()))
                .thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        List<BundleResponse> result = bundleService.createBundleByList(TestUtil.getMockIdPsp(), TestUtil.getMockBundleRequestList());
        assertNotNull(result);
        assertEquals(3, result.size());

        @SuppressWarnings("unchecked")
        ArgumentCaptor<Iterable<Bundle>> captor = ArgumentCaptor.forClass(Iterable.class);
        verify(bundleRepository, times(1)).saveAll(captor.capture());

        Iterator<Bundle> iter = captor.getAllValues().get(0).iterator();
        assertEquals(iter.next().getPspBusinessName(), TestUtil.getMockBundleRequestList().get(0).getPspBusinessName());
        assertEquals(iter.next().getPspBusinessName(), TestUtil.getMockBundleRequestList().get(1).getPspBusinessName());
        assertEquals(iter.next().getPspBusinessName(), TestUtil.getMockBundleRequestList().get(2).getPspBusinessName());
    }

    @Test
    void createBundleByList_ko() {
        // validityDateTo before validityDateFrom
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDate.now().plusDays(8));
        bundleRequest.setValidityDateTo(LocalDate.now().plusDays(7));

        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));
        String idPsp = TestUtil.getMockIdPsp();
        List<BundleRequest> bundleRequestList = List.of(bundleRequest);
        try {
            bundleService.createBundleByList(idPsp, bundleRequestList);
            fail();
        } catch (AppException e) {
            assertEquals(HttpStatus.BAD_REQUEST, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    void getBundleByIdSuccess() {
        Bundle bundle = getMockBundle();
        when(bundleRepository.findById(anyString())).thenReturn(Optional.of(bundle));

        PspBundleDetails result = assertDoesNotThrow(() -> bundleService.getBundleDetailsById("bundleId"));

        assertNotNull(result);
        assertEquals(bundle.getId(), result.getId());
        assertEquals(bundle.getName(), result.getName());
        assertEquals(bundle.getType().name(), result.getType());
    }

    @Test
    void getBundleByIdNotFound() {
        when(bundleRepository.findById(anyString())).thenReturn(Optional.empty());

        AppException e = assertThrows(AppException.class, () -> bundleService.getBundleDetailsById("bundleId"));

        assertNotNull(e);
        assertEquals(HttpStatus.NOT_FOUND, e.getHttpStatus());
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
            bundleService.updateBundle(idPsp, idBundle, bundleRequest, false);
        } catch (AppException e) {
            assertEquals(status, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
    }
}
