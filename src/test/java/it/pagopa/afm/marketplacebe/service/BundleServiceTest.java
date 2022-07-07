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
}
