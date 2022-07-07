package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;

@SpringBootTest
@TestPropertySource(locations = "classpath:application.properties")
@ExtendWith(MockitoExtension.class)
public class BundleServiceTest {
    @Mock
    private BundleRepository bundleRepository;

    @InjectMocks
    private BundleService bundleService;

    @Captor
    ArgumentCaptor<Bundle> bundleArgumentCaptor = ArgumentCaptor.forClass(Bundle.class);

    @Test
    public void shouldDeleteBundle(){
        String TEST_PSP = "test_psp";
        UUID TEST_ID_BUNDLE = UUID.randomUUID();

        Bundle bundle = Bundle.builder()
                .id(TEST_ID_BUNDLE.toString())
                .idPsp(TEST_PSP)
                .name("name")
                .description("description")
                .paymentAmount(100L)
                .minPaymentAmount(1L)
                .maxPaymentAmount(1000L)
                .paymentMethod(PaymentMethod.valueOf("PO"))
                .touchpoint(Touchpoint.valueOf("IO"))
                .type(BundleType.valueOf("PRIVATE"))
                .transferCategoryList(List.of("TEST"))
                .validityDateFrom(null)
                .validityDateTo(null)
                .insertedDate(LocalDateTime.now().minusDays(2))
                .lastUpdatedDate(LocalDateTime.now().minusDays(1))
                .build();

        // Precondition
        Mockito.when(bundleRepository.findById(TEST_ID_BUNDLE.toString(), new PartitionKey(TEST_PSP)))
                .thenReturn(Optional.of(bundle));

        bundleService.removeBundle(TEST_PSP, TEST_ID_BUNDLE.toString());

        verify(bundleRepository).save(bundleArgumentCaptor.capture());

        assertEquals(LocalDate.now(), bundleArgumentCaptor.getValue().getValidityDateTo());
        assertEquals(TEST_ID_BUNDLE.toString(), bundleArgumentCaptor.getValue().getId());
    }
}
