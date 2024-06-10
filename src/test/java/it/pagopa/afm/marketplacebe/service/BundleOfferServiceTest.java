package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.config.MappingsConfiguration;
import it.pagopa.afm.marketplacebe.entity.ArchivedBundleOffer;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.offer.BundleCiOffers;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiBundleId;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.CosmosRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static it.pagopa.afm.marketplacebe.TestUtil.*;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest(classes = {BundleOfferService.class, MappingsConfiguration.class})
class BundleOfferServiceTest {

    @MockBean
    private BundleOfferRepository bundleOfferRepository;

    @MockBean
    private ArchivedBundleOfferRepository archivedBundleOfferRepository;

    @MockBean
    private BundleRepository bundleRepository;

    @MockBean
    private CiBundleRepository ciBundleRepository;

    @MockBean
    private CosmosRepository cosmosRepository;

    @Captor
    private ArgumentCaptor<BundleOffer> bundleOfferArgument;

    @Captor
    private ArgumentCaptor<ArchivedBundleOffer> archivedBundleOfferArgument;

    @Captor
    private ArgumentCaptor<CiBundle> ciBundleArgument;

    @Autowired
    private BundleOfferService bundleOfferService;

    @Test
    void sendBundleOfferOk() {
        Bundle mockBundle = getMockBundle();
        mockBundle.setId("test_bundle_1");
        mockBundle.setValidityDateTo(null);
        mockBundle.setType(BundleType.PRIVATE);
        CiFiscalCodeList mockCiFiscalCodeList = getMockCiFiscalCodeList();

        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(mockBundle));
        var result = bundleOfferService.sendBundleOffer(getMockIdPsp(), mockBundle.getId(), mockCiFiscalCodeList);

        verify(bundleOfferRepository, times(1)).save(bundleOfferArgument.capture());
        assertEquals(1, result.size());
        assertEquals(mockCiFiscalCodeList.getCiFiscalCodeList().get(0), result.get(0).getCiFiscalCode());
        assertEquals(mockBundle.getIdPsp(), bundleOfferArgument.getValue().getIdPsp());
        assertEquals(mockBundle.getId(), bundleOfferArgument.getValue().getIdBundle());
    }

    @Test
    void sendBundleOffer_conflict() {
        Bundle mockBundle = getMockBundle();
        var mockCiBundle = getMockIdPsp();
        mockBundle.setId("test_bundle_1");
        mockBundle.setValidityDateTo(null);
        mockBundle.setType(BundleType.PRIVATE);
        CiFiscalCodeList mockCiFiscalCodeList = getMockCiFiscalCodeList();

        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(mockBundle));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCode(anyString(), anyString())).thenReturn(Optional.of(getMockCiBundle()));
        var id = mockBundle.getId();
        var result = assertThrows(AppException.class,
                () -> bundleOfferService.sendBundleOffer(mockCiBundle, id, mockCiFiscalCodeList));

        assertEquals(result.getHttpStatus(), AppError.BUNDLE_OFFER_CONFLICT.getHttpStatus());
        assertEquals(result.getTitle(), AppError.BUNDLE_OFFER_CONFLICT.getTitle());
    }

    @Test
    void getPspOffers_ok() {
        when(bundleOfferRepository.findByIdPspAndFiscalCodeAndIdBundle(anyString(), anyString(), anyString(), anyInt(), anyInt()))
                .thenReturn(List.of(TestUtil.getMockBundleOffer()));
        when(bundleOfferRepository.getTotalItemsFindByIdPspAndFiscalCodeAndIdBundle(anyString(), anyString(), anyString()))
                .thenReturn(1);

        BundleOffers result = assertDoesNotThrow(() ->
                bundleOfferService.getPspOffers(getMockIdPsp(), "ciTaxCode", "idBundle", 10, 0));

        assertNotNull(result);

        assertNotNull(result.getOffers());
        assertEquals(1, result.getOffers().size());
        assertEquals(getMockIdBundle(), result.getOffers().get(0).getIdBundle());

        assertNotNull(result.getPageInfo());
        assertEquals(10, result.getPageInfo().getLimit());
        assertEquals(0, result.getPageInfo().getPage());
        assertEquals(1, result.getPageInfo().getTotalPages());
        assertEquals(1, result.getPageInfo().getTotalItems());
    }

    @Test
    void getPspOffers_ok_empty_result() {
        when(bundleOfferRepository.findByIdPspAndFiscalCodeAndIdBundle(anyString(), anyString(), anyString(), anyInt(), anyInt()))
                .thenReturn(Collections.emptyList());
        when(bundleOfferRepository.getTotalItemsFindByIdPspAndFiscalCodeAndIdBundle(anyString(), anyString(), anyString()))
                .thenReturn(0);

        BundleOffers result = assertDoesNotThrow(() ->
                bundleOfferService.getPspOffers(getMockIdPsp(), "ciTaxCode", "idBundle", 10, 0));

        assertNotNull(result);

        assertNotNull(result.getOffers());
        assertEquals(0, result.getOffers().size());

        assertNotNull(result.getPageInfo());
        assertEquals(10, result.getPageInfo().getLimit());
        assertEquals(0, result.getPageInfo().getPage());
        assertEquals(0, result.getPageInfo().getTotalPages());
        assertEquals(0, result.getPageInfo().getTotalItems());
    }

    @Test
    void sendBundleOffer_ok_1() {
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.PRIVATE);
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        String idPsp = TestUtil.getMockIdPsp();
        String idBundle = TestUtil.getMockIdBundle();
        CiFiscalCodeList ciFiscalCodeList = getMockCiFiscalCodeList();
        List<BundleOffered> result = bundleOfferService.sendBundleOffer(idPsp, idBundle, ciFiscalCodeList);
        assertNotNull(result);
    }

    @Test
    void sendBundleOffer_ko_1() {
        Bundle bundle = getMockBundle();
        bundle.setType(BundleType.PRIVATE);
        bundle.setValidityDateTo(LocalDate.now());
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        sendBundleOffer_ko(HttpStatus.BAD_REQUEST);
    }

    @Test
    void sendBundleOffer_ko_2() {
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        sendBundleOffer_ko(HttpStatus.CONFLICT);
    }

    @Test
    void sendBundleOffer_ko_3() {
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(LocalDate.now().minusDays(1));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        sendBundleOffer_ko(HttpStatus.BAD_REQUEST);
    }

    @Test
    void sendBundleOffer_ko_4() {
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.empty());
        sendBundleOffer_ko(HttpStatus.NOT_FOUND);
    }

    void sendBundleOffer_ko(HttpStatus status) {
        String idPsp = TestUtil.getMockIdPsp();
        String idBundle = TestUtil.getMockIdBundle();
        CiFiscalCodeList ciFiscalCodeList = getMockCiFiscalCodeList();
        try {
            bundleOfferService.sendBundleOffer(idPsp, idBundle, ciFiscalCodeList);
            fail();
        } catch (AppException e) {
            assertEquals(status, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    void removeBundleOffer_ok_1() {
        BundleOffer bundleOffer = TestUtil.getMockBundleOffer();
        when(bundleOfferRepository.findById(anyString())).thenReturn(Optional.of(bundleOffer));

        String idPsp = TestUtil.getMockIdPsp();
        String idBundle = TestUtil.getMockIdBundle();
        try {
            bundleOfferService.removeBundleOffer(idPsp, idBundle, bundleOffer.getId());
            assertTrue(true);
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    void removeBundleOffer_ko_1() {
        when(bundleOfferRepository.findById(anyString())).thenReturn(Optional.empty());
        BundleOffer bundleOffer = TestUtil.getMockBundleOffer();
        removeBundleOffer_ko(bundleOffer, HttpStatus.NOT_FOUND);
    }

    @Test
    void removeBundleOffer_ko_2() {
        BundleOffer bundleOffer = TestUtil.getMockBundleOffer();
        bundleOffer.setIdPsp("UNKNOWN");
        when(bundleOfferRepository.findById(anyString())).thenReturn(Optional.of(bundleOffer));
        removeBundleOffer_ko(bundleOffer, HttpStatus.BAD_REQUEST);
    }

    @Test
    void removeBundleOffer_ko_3() {
        BundleOffer bundleOffer = TestUtil.getMockBundleOffer();
        bundleOffer.setIdBundle("UNKNOWN");
        when(bundleOfferRepository.findById(anyString())).thenReturn(Optional.of(bundleOffer));
        removeBundleOffer_ko(bundleOffer, HttpStatus.BAD_REQUEST);
    }

    void removeBundleOffer_ko(BundleOffer bundleOffer, HttpStatus status) {
        String idPsp = TestUtil.getMockIdPsp();
        String idBundle = TestUtil.getMockIdBundle();
        String idBundleOffer = bundleOffer.getId();
        try {
            bundleOfferService.removeBundleOffer(idPsp, idBundle, idBundleOffer);
            fail();
        } catch (AppException e) {
            assertEquals(status, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
    }

    @ParameterizedTest
    @NullSource    // pass a null value
    @ValueSource(strings = {"1234567901"})
    void getCiOffers_ok_1(String idPsp) {
        String ciFiscalCode = TestUtil.getMockCiFiscalCode();
        BundleOffer bundleOffer = TestUtil.getMockBundleOffer();
        when(bundleOfferRepository.findByIdPspAndFiscalCodeAndIdBundles(ciFiscalCode, idPsp, null, 10, 1))
                .thenReturn(List.of(bundleOffer));
        when(bundleOfferRepository.getTotalItemsFindByIdPspAndFiscalCodeAndIdBundles(idPsp, ciFiscalCode, null))
                .thenReturn(1);

        BundleCiOffers result = assertDoesNotThrow(() -> bundleOfferService.getCiOffers(idPsp, ciFiscalCode, null, 10, 1));

        assertNotNull(result);

        verify(cosmosRepository, never()).getBundlesByNameAndPSPBusinessName(anyString(), eq(null), eq(BundleType.PRIVATE.name()));
    }

    @Test
    void getCiOffers_ok_with_bundle_name_filter() {
        String ciFiscalCode = TestUtil.getMockCiFiscalCode();
        BundleOffer bundleOffer = TestUtil.getMockBundleOffer();
        List<Bundle> mockBundleList = getMockBundleList();
        List<String> mockIdBundleList = mockBundleList.parallelStream().map(Bundle::getId).toList();

        when(cosmosRepository.getBundlesByNameAndPSPBusinessName("name", null, BundleType.PRIVATE.name()))
                .thenReturn(mockBundleList);
        when(bundleOfferRepository.findByIdPspAndFiscalCodeAndIdBundles(null, ciFiscalCode, mockIdBundleList, 10, 1))
                .thenReturn(List.of(bundleOffer));
        when(bundleOfferRepository.getTotalItemsFindByIdPspAndFiscalCodeAndIdBundles(null, ciFiscalCode, mockIdBundleList))
                .thenReturn(1);

        BundleCiOffers result = assertDoesNotThrow(() -> bundleOfferService.getCiOffers(ciFiscalCode, null, "name", 10, 1));

        assertNotNull(result);
    }

    @Test
    void removeBundleOfferOk() {

        BundleOffer offerToArchive = getMockBundleOffer();
        when(bundleOfferRepository.findById(getMockBundleOfferId()))
                .thenReturn(Optional.of(offerToArchive));

        bundleOfferService.removeBundleOffer(getMockIdPsp(), getMockIdBundle(), getMockBundleOfferId());

        verify(archivedBundleOfferRepository, times(1)).save(archivedBundleOfferArgument.capture());
        verify(bundleOfferRepository, times(1)).delete(bundleOfferArgument.capture());

        assertEquals(getMockArchivedBundleOffer(offerToArchive).getId(), archivedBundleOfferArgument.getValue().getId());
        assertEquals(offerToArchive.getId(), bundleOfferArgument.getValue().getId());
    }

    @Test
    void acceptOfferSuccessNoUpdate() {
        CiBundle ciBundle = getMockCiBundle();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundleOffer()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundle()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), getMockCiFiscalCode()))
                .thenReturn(Optional.empty());
        when(ciBundleRepository.save(any())).thenReturn(ciBundle);

        CiBundleId result = assertDoesNotThrow(() -> bundleOfferService.acceptOffer(
                        getMockCiFiscalCode(),
                        getMockBundleOfferId(),
                        Collections.singletonList(getMockCiBundleAttributeModel()))
        );

        assertNotNull(result);
        assertEquals(ciBundle.getId(), result.getId());

        verify(ciBundleRepository, times(1)).save(ciBundleArgument.capture());
        assertEquals(getMockBundleOffer().getIdBundle(), ciBundleArgument.getValue().getIdBundle());
    }

    @Test
    void acceptOfferSuccessValidityWithoutAttributes() {
        CiBundle ciBundle = getMockCiBundle();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundleOffer()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundle()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), getMockCiFiscalCode()))
                .thenReturn(Optional.empty());
        when(ciBundleRepository.save(any())).thenReturn(ciBundle);

        CiBundleId result = assertDoesNotThrow(() -> bundleOfferService.acceptOffer(
                        getMockCiFiscalCode(),
                        getMockBundleOfferId(),
                        Collections.emptyList())
        );

        assertNotNull(result);
        assertEquals(ciBundle.getId(), result.getId());

        verify(ciBundleRepository, times(1)).save(ciBundleArgument.capture());
        assertEquals(getMockBundleOffer().getIdBundle(), ciBundleArgument.getValue().getIdBundle());
    }

    @Test
    void acceptOfferFailBundleOfferNotFound() {
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(LocalDate.now());
        String mockCiFiscalCode = getMockCiFiscalCode();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.empty());

        String mockBundleOfferId = getMockBundleOfferId();
        List<CiBundleAttributeModel> attributes = Collections.singletonList(getMockCiBundleAttributeModel());
        var e = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId, attributes));

        assertNotNull(e);
        assertEquals(AppError.BUNDLE_OFFER_NOT_FOUND.getHttpStatus(), e.getHttpStatus());
        assertEquals(AppError.BUNDLE_OFFER_NOT_FOUND.getTitle(), e.getTitle());

        verify(bundleRepository, never()).findById(anyString(), any(PartitionKey.class));
        verify(bundleOfferRepository, never())
                .findByIdBundleAndCiFiscalCodeAndAcceptedDateIsNullAndRejectionDateIsNull(anyString(), anyString());
        verify(ciBundleRepository, never()).findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), mockCiFiscalCode);
        verify(ciBundleRepository, never()).save(any());
    }

    @Test
    void acceptOfferFailBundleNotFound() {
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(LocalDate.now());
        String mockCiFiscalCode = getMockCiFiscalCode();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundleOffer()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.empty());

        String mockBundleOfferId = getMockBundleOfferId();
        List<CiBundleAttributeModel> attributes = Collections.singletonList(getMockCiBundleAttributeModel());
        var e = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId, attributes));

        assertNotNull(e);
        assertEquals(AppError.BUNDLE_NOT_FOUND.getHttpStatus(), e.getHttpStatus());
        assertEquals(AppError.BUNDLE_NOT_FOUND.getTitle(), e.getTitle());

        verify(bundleOfferRepository, never())
                .findByIdBundleAndCiFiscalCodeAndAcceptedDateIsNullAndRejectionDateIsNull(anyString(), anyString());
        verify(ciBundleRepository, never()).findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), mockCiFiscalCode);
        verify(ciBundleRepository, never()).save(any());
    }

    @Test
    void acceptOfferFailBundleAlreadyDeleted() {
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(LocalDate.now());
        String mockCiFiscalCode = getMockCiFiscalCode();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundleOffer()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));

        String mockBundleOfferId = getMockBundleOfferId();
        List<CiBundleAttributeModel> attributes = Collections.singletonList(getMockCiBundleAttributeModel());
        var e = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId, attributes));

        assertNotNull(e);
        assertEquals(AppError.BUNDLE_BAD_REQUEST.getHttpStatus(), e.getHttpStatus());
        assertEquals(AppError.BUNDLE_BAD_REQUEST.getTitle(), e.getTitle());

        verify(bundleOfferRepository, never())
                .findByIdBundleAndCiFiscalCodeAndAcceptedDateIsNullAndRejectionDateIsNull(anyString(), anyString());
        verify(ciBundleRepository, never()).findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), mockCiFiscalCode);
        verify(ciBundleRepository, never()).save(any());
    }

    @Test
    void acceptOfferFailConflictDuplicatedOffer() {
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        bundle.setValidityDateFrom(null);
        String mockCiFiscalCode = getMockCiFiscalCode();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundleOffer()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(bundle));
        when(bundleOfferRepository.findByIdBundleAndCiFiscalCodeAndAcceptedDateIsNullAndRejectionDateIsNull(anyString(), anyString()))
                .thenReturn(Optional.of(getMockBundleOffer()));

        String mockBundleOfferId = UUID.randomUUID().toString();
        List<CiBundleAttributeModel> attributes = Collections.singletonList(getMockCiBundleAttributeModel());
        var e = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId, attributes));

        assertNotNull(e);
        assertEquals(AppError.BUNDLE_OFFER_CONFLICT.getHttpStatus(), e.getHttpStatus());
        assertEquals(AppError.BUNDLE_OFFER_CONFLICT.getTitle(), e.getTitle());

        verify(ciBundleRepository, never()).findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(any(), anyString());
        verify(ciBundleRepository, never()).save(any());
    }

    @Test
    void acceptOfferFailAlreadyAccepted() {
        BundleOffer mockBundleOffer = getMockBundleOffer();
        mockBundleOffer.setAcceptedDate(LocalDateTime.now());
        String mockCiFiscalCode = getMockCiFiscalCode();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(mockBundleOffer));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundle()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), getMockCiFiscalCode()))
                .thenReturn(Optional.empty());
        when(ciBundleRepository.save(any())).thenReturn(getMockCiBundle());

        String mockBundleOfferId = getMockBundleOfferId();
        List<CiBundleAttributeModel> attributes = Collections.singletonList(getMockCiBundleAttributeModel());
        var e = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId, attributes));

        assertNotNull(e);
        assertEquals(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED.getHttpStatus(), e.getHttpStatus());
        assertEquals(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED.getTitle(), e.getTitle());

        verify(ciBundleRepository, never()).save(any());
    }

    @Test
    void acceptOfferFailAlreadyAcceptedAndCIBundleIsPresent() {
        String mockCiFiscalCode = getMockCiFiscalCode();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundleOffer()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundle()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), mockCiFiscalCode))
                .thenReturn(Optional.of(getMockCiBundle()));
        when(ciBundleRepository.save(any())).thenReturn(getMockCiBundle());

        String mockBundleOfferId = getMockBundleOfferId();
        List<CiBundleAttributeModel> attributes = Collections.singletonList(getMockCiBundleAttributeModel());
        var e = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId, attributes));

        assertNotNull(e);
        assertEquals(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED.getHttpStatus(), e.getHttpStatus());
        assertEquals(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED.getTitle(), e.getTitle());

        verify(ciBundleRepository, never()).save(any());
    }

    @Test
    void acceptOfferFailAlreadyRejected() {
        BundleOffer mockBundleOffer = getMockBundleOffer();
        mockBundleOffer.setRejectionDate(LocalDateTime.now());
        String mockCiFiscalCode = getMockCiFiscalCode();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(mockBundleOffer));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundle()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), getMockCiFiscalCode()))
                .thenReturn(Optional.empty());

        String mockBundleOfferId = getMockBundleOfferId();
        List<CiBundleAttributeModel> attributes = Collections.singletonList(getMockCiBundleAttributeModel());
        var e = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId, attributes));

        assertNotNull(e);
        assertEquals(AppError.BUNDLE_OFFER_ALREADY_REJECTED.getHttpStatus(), e.getHttpStatus());
        assertEquals(AppError.BUNDLE_OFFER_ALREADY_REJECTED.getTitle(), e.getTitle());

        verify(ciBundleRepository, never()).save(any());
    }

    @Test
    void acceptOfferFailAttributeWithInvalidPaymentAmount() {
        BundleOffer mockBundleOffer = getMockBundleOffer();
        String mockCiFiscalCode = getMockCiFiscalCode();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(mockBundleOffer));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundle()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), getMockCiFiscalCode()))
                .thenReturn(Optional.empty());

        String mockBundleOfferId = getMockBundleOfferId();
        CiBundleAttributeModel attributeModel = getMockCiBundleAttributeModel();
        attributeModel.setMaxPaymentAmount(10000000000000000L);
        List<CiBundleAttributeModel> attributes = Collections.singletonList(attributeModel);
        var e = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId, attributes));

        assertNotNull(e);
        assertEquals(AppError.BUNDLE_OFFER_BAD_REQUEST.getHttpStatus(), e.getHttpStatus());
        assertEquals(AppError.BUNDLE_OFFER_BAD_REQUEST.getTitle(), e.getTitle());

        verify(ciBundleRepository, never()).save(any());
    }

    @Test
    void acceptOfferFailAttributeWithMultipleAttributeAndTransferCategoryNull() {
        BundleOffer mockBundleOffer = getMockBundleOffer();
        String mockCiFiscalCode = getMockCiFiscalCode();

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(mockBundleOffer));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(getMockBundle()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), getMockCiFiscalCode()))
                .thenReturn(Optional.empty());

        String mockBundleOfferId = getMockBundleOfferId();
        CiBundleAttributeModel attributeModel = getMockCiBundleAttributeModel();
        attributeModel.setTransferCategory(null);
        List<CiBundleAttributeModel> attributes = List.of(attributeModel, getMockCiBundleAttributeModel());
        var e = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId, attributes));

        assertNotNull(e);
        assertEquals(AppError.BUNDLE_OFFER_BAD_ATTRIBUTE.getHttpStatus(), e.getHttpStatus());
        assertEquals(AppError.BUNDLE_OFFER_BAD_ATTRIBUTE.getTitle(), e.getTitle());

        verify(ciBundleRepository, never()).save(any());
    }

    @Test
    void rejectOfferOk() {

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundleOffer()));

        when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundle()));

        bundleOfferService.rejectOffer(getMockCiFiscalCode(), getMockBundleOfferId());

        verify(archivedBundleOfferRepository, times(1)).save(archivedBundleOfferArgument.capture());
        verify(bundleOfferRepository, times(1)).delete(bundleOfferArgument.capture());

        assertEquals(getMockBundleOffer().getId(), bundleOfferArgument.getValue().getId());
        assertEquals(getMockBundleOffer().getId(), archivedBundleOfferArgument.getValue().getId());
    }

    @Test
    void sendOfferForPublicBundleKO() {
        Bundle mockBundle = getMockBundle();
        mockBundle.setId("test_bundle_1");
        mockBundle.setValidityDateTo(null);
        mockBundle.setType(BundleType.GLOBAL);
        CiFiscalCodeList mockCiFiscalCodeList = getMockCiFiscalCodeList();

        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(mockBundle));

        String idPsp = getMockIdPsp();
        String idBundle = mockBundle.getId();

        var exc = assertThrows(AppException.class,
                () -> bundleOfferService.sendBundleOffer(idPsp, idBundle, mockCiFiscalCodeList));

        assertEquals(HttpStatus.CONFLICT, exc.getHttpStatus());
    }

}
