package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.entity.*;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static it.pagopa.afm.marketplacebe.TestUtil.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@SpringBootTest
class BundleOfferServiceTest {

    @MockBean
    BundleOfferRepository bundleOfferRepository;

    @MockBean
    ArchivedBundleOfferRepository archivedBundleOfferRepository;

    @MockBean
    BundleRepository bundleRepository;

    @MockBean
    CiBundleRepository ciBundleRepository;

    @Captor
    ArgumentCaptor<BundleOffer> bundleOfferArgument;

    @Captor
    ArgumentCaptor<ArchivedBundleOffer> archivedBundleOfferArgument;

    @Captor
    ArgumentCaptor<CiBundle> ciBundleArgument;

    @Autowired
    @InjectMocks
    BundleOfferService bundleOfferService;

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
        when(bundleOfferRepository.findByIdPsp(anyString())).thenReturn(List.of(TestUtil.getMockBundleOffer()));

        BundleOffers result = bundleOfferService.getPspOffers(getMockIdPsp());
        assertNotNull(result);
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
        BundleOffer bundleOffer = TestUtil.getMockBundleOffer();
        when(bundleOfferRepository.findByCiFiscalCode(anyString())).thenReturn(List.of(bundleOffer));
        when(bundleOfferRepository.findByIdPsp(anyString(), any(PartitionKey.class))).thenReturn(List.of(bundleOffer));

        String ciFiscalCode = TestUtil.getMockCiFiscalCode();
        try {
            bundleOfferService.getCiOffers(ciFiscalCode, idPsp);
            assertTrue(true);
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    void getOffersTest() {

        when(bundleOfferRepository.findByIdPsp(getMockIdPsp()))
                .thenReturn(getMockBundleOfferList());

        var result = bundleOfferService.getPspOffers(getMockIdPsp());

        verify(bundleOfferRepository, times(1)).findByIdPsp(getMockIdPsp());
        assertEquals(1, result.getOffers().size());
        assertEquals(getMockIdBundle(), result.getOffers().get(0).getIdBundle());
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
    void restGetOffersByCiFiscalCodeOk() {
        when(bundleOfferRepository.findByCiFiscalCode(getMockCiFiscalCode()))
                .thenReturn(getMockBundleOfferList());

        var result = bundleOfferService.getCiOffers(getMockCiFiscalCode(), null);

        assertEquals(1, result.getOffers().size());
    }

    @Test
    void restGetOffersByidPspCodeOk() {
        when(bundleOfferRepository.findByIdPsp(anyString(), any(PartitionKey.class)))
                .thenReturn(getMockBundleOfferList());

        var result = bundleOfferService.getCiOffers(null, getMockIdPsp());

        assertEquals(1, result.getOffers().size());
    }

    @Test
    void acceptOfferNoUpdateOk() {
        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundleOffer()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), getMockCiFiscalCode()))
                .thenReturn(Optional.empty());
        when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundle()));

        when(ciBundleRepository.save(any())).thenReturn(
                getMockCiBundle()
        );

        bundleOfferService.acceptOffer(getMockCiFiscalCode(), getMockBundleOfferId());

        verify(ciBundleRepository, times(1)).save(ciBundleArgument.capture());
        assertEquals(getMockBundleOffer().getIdBundle(), ciBundleArgument.getValue().getIdBundle());
    }

    @Test
    void acceptOfferConflict() {
        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundleOffer()));
        String mockCiFiscalCode = getMockCiFiscalCode();
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), mockCiFiscalCode))
                .thenReturn(Optional.of(getMockCiBundle()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundle()));

        when(ciBundleRepository.save(any())).thenReturn(
                getMockCiBundle()
        );

        String mockBundleOfferId = getMockBundleOfferId();
        var exception = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId));

        assertEquals(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED.getHttpStatus(), exception.getHttpStatus());
        assertEquals(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED.getTitle(), exception.getTitle());
    }

    @Test
    void acceptOfferConflict2() {
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        bundle.setValidityDateFrom(null);

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundleOffer()));
        String mockCiFiscalCode = getMockCiFiscalCode();
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), mockCiFiscalCode))
                .thenReturn(Optional.of(getMockCiBundle()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.save(any())).thenReturn(
                getMockCiBundle()
        );

        String mockBundleOfferId = getMockBundleOfferId();
        var exception = assertThrows(AppException.class,
                () -> bundleOfferService.acceptOffer(mockCiFiscalCode, mockBundleOfferId));

        assertEquals(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED.getHttpStatus(), exception.getHttpStatus());
        assertEquals(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED.getTitle(), exception.getTitle());
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
