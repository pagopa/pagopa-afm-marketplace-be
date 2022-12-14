package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentType;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentTypes;
import it.pagopa.afm.marketplacebe.model.touchpoint.Touchpoint;
import it.pagopa.afm.marketplacebe.model.touchpoint.TouchpointRequest;
import it.pagopa.afm.marketplacebe.model.touchpoint.Touchpoints;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.PaymentTypeRepository;
import it.pagopa.afm.marketplacebe.repository.TouchpointRepository;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
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
