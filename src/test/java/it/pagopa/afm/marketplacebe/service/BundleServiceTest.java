package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.MarketplaceBeApplication;
import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static it.pagopa.afm.marketplacebe.TestUtil.getMockBundle;
import static it.pagopa.afm.marketplacebe.TestUtil.getMockBundleRequest;
import static it.pagopa.afm.marketplacebe.TestUtil.getMockCiBundle;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;

@SpringBootTest
class BundleServiceTest {
    @MockBean
    private BundleRepository bundleRepository;

    @MockBean
    private CiBundleRepository ciBundleRepository;

    @MockBean
    private BundleRequestRepository bundleRequestRepository;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.bundle.BundleResponse;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;



@SpringBootTest(classes = MarketplaceBeApplication.class)
class BundleServiceTest {

    @MockBean
    private BundleRepository bundleRepository;

    @Autowired
    @InjectMocks
    private BundleService bundleService;

    @Captor
    ArgumentCaptor<Bundle> bundleArgumentCaptor = ArgumentCaptor.forClass(Bundle.class);

    @Test
    void shouldDeleteBundle(){
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
    void createBundle_ok_1() {
        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(Collections.emptyList());

        when(bundleRepository.findByName(anyString(), any())).thenReturn(Optional.empty());
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

        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        when(bundleRepository.findByName(anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_3() {
        // same (payment method, touchpoint, type, transferCategoryList, payment amount range), different validityDate
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDateTime.now().plusDays(8));
        bundleRequest.setValidityDateTo(LocalDateTime.now().plusDays(10));
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        when(bundleRepository.findByName(anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_4() {
        // same (payment method, touchpoint, type, transferCategoryList), different payment amount range and validityDate
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDateTime.now().plusDays(8));
        bundleRequest.setValidityDateTo(LocalDateTime.now().plusDays(10));
        List<Bundle> bundles = TestUtil.getMockBundleSameConfigurationDifferentPaymentAmountRange();

        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        when(bundleRepository.findByName(anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ok_5() {
        // same (payment method, touchpoint, transferCategoryList, payment amount range), different type
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setType(BundleType.PUBLIC);

        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(Collections.emptyList());

        when(bundleRepository.findByName(anyString(), any())).thenReturn(Optional.empty());
        when(bundleRepository.save(any(Bundle.class))).thenReturn(TestUtil.getMockBundle());

        BundleResponse result = bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
        assertNotNull(result);
    }

    @Test
    void createBundle_ko_1() {
        // validityDateFrom > validityDateTo
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDateTime.now());
        bundleRequest.setValidityDateTo(LocalDateTime.now().minusDays(1));

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_2() {
        // same bundle configuration
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_3() {
        // same (payment method, touchpoint, type, transferCategoryList), different payment amount range
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        List<Bundle> bundles = TestUtil.getMockBundleSameConfigurationDifferentPaymentAmountRange();

        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_4() {
        // same (payment method, touchpoint, type, transferCategoryList, payment amount range), different validityDate
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setValidityDateFrom(LocalDateTime.now().plusDays(8));
        bundleRequest.setValidityDateTo(LocalDateTime.now().plusDays(10));
        List<Bundle> bundles = TestUtil.getMockBundleWithoutValidityDateTo();

        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_5() {
        // same (payment method, touchpoint, type, transferCategoryList), payment amount range overlapping
        BundleRequest bundleRequest = TestUtil.getMockBundleRequest();
        bundleRequest.setMaxPaymentAmount(1500L);
        List<Bundle> bundles = TestUtil.getMockBundleSameConfiguration();

        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(bundles);

        createBundle_ko(bundleRequest, HttpStatus.BAD_REQUEST);
    }

    @Test
    void createBundle_ko_6() {
        // bundle name conflict
        when(bundleRepository.findByTypeAndPaymentMethodAndTouchpoint(
                any(BundleType.class), any(PaymentMethod.class), any(Touchpoint.class))).thenReturn(Collections.emptyList());

        when(bundleRepository.findByName(anyString(), any())).thenReturn(Optional.of(TestUtil.getMockBundle()));

        createBundle_ko(TestUtil.getMockBundleRequest(), HttpStatus.CONFLICT);
    }

    private void createBundle_ko(BundleRequest bundleRequest, HttpStatus status) {
        try {
            bundleService.createBundle(TestUtil.getMockIdPsp(), bundleRequest);
            fail();
        } catch (AppException e) {
            assertEquals(status, e.getHttpStatus());
        } catch (Exception e) {
            fail();
        }
    }

}
