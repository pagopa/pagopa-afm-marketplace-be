package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.*;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.util.Optional;

import static it.pagopa.afm.marketplacebe.TestUtil.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
    void getOffersTest(){

        when(bundleOfferRepository.findByIdPsp(getMockIdPsp()))
                .thenReturn(getMockBundleOfferList());

        var result = bundleOfferService.getPspOffers(getMockIdPsp());

        verify(bundleOfferRepository, times(1)).findByIdPsp(getMockIdPsp());
        assertEquals(1, result.getOffers().size());
        assertEquals(getMockIdBundle(), result.getOffers().get(0).getIdBundle());
    }

    @Test
    void removeBundleOfferOk(){

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
    void restGetOffersByCiFiscalCodeOk(){
        when(bundleOfferRepository.findByCiFiscalCode(getMockCiFiscalCode()))
                .thenReturn(getMockBundleOfferList());

        var result = bundleOfferService.getCiOffers(getMockCiFiscalCode(), null);

        assertEquals(1, result.getOffers().size());
    }

    @Test
    void restGetOffersByidPspCodeOk(){
        when(bundleOfferRepository.findByIdPsp(anyString(), any(PartitionKey.class)))
                .thenReturn(getMockBundleOfferList());

        var result = bundleOfferService.getCiOffers(null, getMockIdPsp());

        assertEquals(1, result.getOffers().size());
    }

    @Test
    void acceptOfferNoUpdateOk(){
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
    void acceptOfferWithUpdateOk(){
        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundleOffer()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), getMockCiFiscalCode()))
                .thenReturn(Optional.of(getMockCiBundle()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundle()));

        when(ciBundleRepository.save(any())).thenReturn(
                getMockCiBundle()
        );

        bundleOfferService.acceptOffer(getMockCiFiscalCode(), getMockBundleOfferId());

        verify(ciBundleRepository, times(2)).save(ciBundleArgument.capture());
        verify(archivedBundleOfferRepository, times(1)).save(archivedBundleOfferArgument.capture());

        assertEquals(getMockBundleOffer().getIdBundle(), archivedBundleOfferArgument.getValue().getIdBundle());
    }

    @Test
    void acceptOfferWithUpdateNoValidityDate(){
        Bundle bundle = getMockBundle();
        bundle.setValidityDateTo(null);
        bundle.setValidityDateFrom(null);

        when(bundleOfferRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(getMockBundleOffer()));
        when(ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(getMockIdBundle(), getMockCiFiscalCode()))
                .thenReturn(Optional.of(getMockCiBundle()));
        when(bundleRepository.findById(anyString(), any(PartitionKey.class)))
                .thenReturn(Optional.of(bundle));

        when(ciBundleRepository.save(any())).thenReturn(
                getMockCiBundle()
        );

        bundleOfferService.acceptOffer(getMockCiFiscalCode(), getMockBundleOfferId());

        verify(ciBundleRepository, times(2)).save(ciBundleArgument.capture());
        verify(archivedBundleOfferRepository, times(1)).save(archivedBundleOfferArgument.capture());

        assertEquals(getMockBundleOffer().getIdBundle(), archivedBundleOfferArgument.getValue().getIdBundle());
    }

    @Test
    void rejectOfferOk(){

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
    void sendOfferForPublicBundleKO(){
        Bundle mockBundle = getMockBundle();
        mockBundle.setId("test_bundle_1");
        mockBundle.setValidityDateTo(null);
        mockBundle.setType(BundleType.GLOBAL);
        CiFiscalCodeList mockCiFiscalCodeList = getMockCiFiscalCodeList();

        when(bundleRepository.findById(anyString(), any(PartitionKey.class))).thenReturn(Optional.of(mockBundle));

        var exc = assertThrows(AppException.class,
            () -> bundleOfferService.sendBundleOffer(getMockIdPsp(), mockBundle.getId(), mockCiFiscalCodeList));

        assertEquals(HttpStatus.CONFLICT, exc.getHttpStatus());
    }

}
