package it.pagopa.afm.marketplacebe.service;

import com.azure.spring.data.cosmos.core.CosmosTemplate;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.model.paymentmethods.FeeRange;
import it.pagopa.afm.marketplacebe.model.paymentmethods.enums.PaymentMethodDisabledReason;
import it.pagopa.afm.marketplacebe.model.paymentmethods.enums.PaymentMethodGroup;
import it.pagopa.afm.marketplacebe.model.paymentmethods.enums.PaymentMethodStatus;
import it.pagopa.afm.marketplacebe.repository.PaymentMethodRepository;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.io.IOException;
import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@SpringBootTest
class PaymentMethodsServiceTest {

    @Autowired
    @InjectMocks
    PaymentMethodsService paymentMethodsService;

    @MockBean
    CosmosTemplate cosmosTemplate;

    @MockBean
    PaymentMethodRepository paymentMethodRepository;


    @Test
    void getPaymentMethods() {

        when(paymentMethodRepository
                .findAll()).thenReturn(List.of(PaymentMethod.builder()
                .paymentMethodId("PAYPAL")
                .status(PaymentMethodStatus.MAINTENANCE)
                .group(PaymentMethodGroup.PPAL)
                .target(List.of("user"))
                .validityDateFrom(LocalDate.now().minusDays(1))
                .rangeAmount(FeeRange.builder()
                        .min(5L)
                        .max(1000L)
                        .build())
                .build()));

        var result = paymentMethodsService.getPaymentMethods();
        assertEquals(1, result.size());
    }

    @Test
    void getPaymentMethod() {
        when(paymentMethodRepository
                .findByPaymentMethodId(anyString())).thenReturn(List.of(PaymentMethod.builder()
                .paymentMethodId("PAYPAL")
                .status(PaymentMethodStatus.MAINTENANCE)
                .group(PaymentMethodGroup.PPAL)
                .target(List.of("user"))
                .validityDateFrom(LocalDate.now().minusDays(1))
                .rangeAmount(FeeRange.builder()
                        .min(5L)
                        .max(1000L)
                        .build())
                .build()));

        var result = paymentMethodsService.getPaymentMethod("PAYPAL");
        assertEquals("PAYPAL", result.getPaymentMethodId());
    }

    @Test
    void createPaymentMethod() {
        paymentMethodsService.createPaymentMethod(PaymentMethod.builder()
                .paymentMethodId("PAYPAL")
                .status(PaymentMethodStatus.MAINTENANCE)
                .group(PaymentMethodGroup.PPAL)
                .target(List.of("user"))
                .validityDateFrom(LocalDate.now().minusDays(1))
                .rangeAmount(FeeRange.builder()
                        .min(5L)
                        .max(1000L)
                        .build())
                .build());
        verify(paymentMethodRepository, times(1)).save(any(PaymentMethod.class));
    }

    @Test
    void updatePaymentMethod() {
        when(paymentMethodRepository
                .findByPaymentMethodId(anyString())).thenReturn(List.of(PaymentMethod.builder()
                .paymentMethodId("PAYPAL")
                .status(PaymentMethodStatus.MAINTENANCE)
                .group(PaymentMethodGroup.PPAL)
                .target(List.of("user"))
                .validityDateFrom(LocalDate.now().minusDays(1))
                .rangeAmount(FeeRange.builder()
                        .min(5L)
                        .max(1000L)
                        .build())
                .build()));
        paymentMethodsService.updatePaymentMethod("PAYPAL", PaymentMethod.builder()
                .paymentMethodId("PAYPAL")
                .status(PaymentMethodStatus.MAINTENANCE)
                .group(PaymentMethodGroup.PPAL)
                .target(List.of("user"))
                .validityDateFrom(LocalDate.now().minusDays(1))
                .rangeAmount(FeeRange.builder()
                        .min(5L)
                        .max(1000L)
                        .build())
                .build());
        verify(paymentMethodRepository, times(1)).save(any(PaymentMethod.class));

    }

    @Test
    void deletePaymentMethod() {

        when(paymentMethodRepository
                .findByPaymentMethodId(anyString())).thenReturn(List.of(PaymentMethod.builder()
                .paymentMethodId("PAYPAL")
                .status(PaymentMethodStatus.MAINTENANCE)
                .group(PaymentMethodGroup.PPAL)
                .target(List.of("user"))
                .validityDateFrom(LocalDate.now().minusDays(1))
                .rangeAmount(FeeRange.builder()
                        .min(5L)
                        .max(1000L)
                        .build())
                .build()));

        paymentMethodsService.deletePaymentMethod("PAYPAL");
        verify(paymentMethodRepository, times(1)).delete(any(PaymentMethod.class));

    }
}
