package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentType;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentTypes;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.PaymentTypeRepository;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@SpringBootTest
class PaymentTypeServiceTest {

    @Captor
    ArgumentCaptor<it.pagopa.afm.marketplacebe.entity.Touchpoint> touchpointArgumentCaptor = ArgumentCaptor.forClass(it.pagopa.afm.marketplacebe.entity.Touchpoint.class);
    @MockBean
    private PaymentTypeRepository paymentTypeRepository;

    @MockBean
    private BundleRepository bundleRepository;
    @Autowired
    @InjectMocks
    private PaymentTypeService paymentTypeService;

    @Test
    void shouldGetPaymentTypes() {

        // Precondition
        when(paymentTypeRepository.findAll()).thenReturn(List.of(TestUtil.getMockPaymentType()));

        // Tests
        PaymentTypes paymentTypes = paymentTypeService.getPaymentTypes();

        // Assertions
        assertEquals(1, paymentTypes.getPaymentTypes().size());
    }

    @Test
    void shouldGetPaymentType() {
        it.pagopa.afm.marketplacebe.entity.PaymentType paymentTypeMock = TestUtil.getMockPaymentType();

        // Precondition
        when(paymentTypeRepository.findByName(paymentTypeMock.getName())).thenReturn(Optional.of(paymentTypeMock));

        // Tests
        PaymentType paymentType = paymentTypeService.getPaymentType(paymentTypeMock.getName());

        // Assertions
        assertEquals(paymentTypeMock.getName(), paymentType.getPaymentType());
    }

}
