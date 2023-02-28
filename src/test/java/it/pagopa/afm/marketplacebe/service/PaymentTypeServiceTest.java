package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentType;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentTypes;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.PaymentTypeRepository;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static it.pagopa.afm.marketplacebe.TestUtil.getMockPaymentTypeList;
import static it.pagopa.afm.marketplacebe.TestUtil.getMockPaymentTypeListForCreate;
import static it.pagopa.afm.marketplacebe.exception.AppError.PAYMENT_TYPE_NOT_DELETABLE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@SpringBootTest
class PaymentTypeServiceTest {

    @Captor
    ArgumentCaptor<it.pagopa.afm.marketplacebe.entity.Touchpoint> touchpointArgumentCaptor = ArgumentCaptor.forClass(it.pagopa.afm.marketplacebe.entity.Touchpoint.class);

    @Captor
    ArgumentCaptor<Collection<it.pagopa.afm.marketplacebe.entity.PaymentType>> paymentTypeArgumentCaptor = ArgumentCaptor.forClass(Collection.class);
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
        assertEquals(1, paymentTypes.getPaymentTypeList().size());
    }

    @Test
    void shouldGetPaymentType() {
        it.pagopa.afm.marketplacebe.entity.PaymentType paymentTypeMock = TestUtil.getMockPaymentType();

        // Precondition
        when(paymentTypeRepository.findByName(paymentTypeMock.getName())).thenReturn(Optional.of(paymentTypeMock));

        // Tests
        PaymentType paymentType = paymentTypeService.getPaymentType(paymentTypeMock.getName());

        // Assertions
        assertEquals(paymentTypeMock.getName(), paymentType.getName());
    }

    @Test
    void shouldUploadPaymentTypeList() {
        List<String> paymentTypeList = getMockPaymentTypeListForCreate();
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypeEntityList = getMockPaymentTypeList();
        it.pagopa.afm.marketplacebe.entity.PaymentType paymentType = TestUtil.getMockPaymentType();
        // preconditions
        when(paymentTypeRepository.findByName(anyString())).thenReturn(Optional.of(paymentType));
        when(bundleRepository.findByPaymentType(paymentType.getName())).thenReturn(List.of());
        when(paymentTypeRepository.saveAll(Mockito.any())).thenReturn(paymentTypeEntityList);
        // tests
        paymentTypeService.uploadPaymentTypeByList(paymentTypeList);
        verify(paymentTypeRepository).saveAll(paymentTypeArgumentCaptor.capture());
        // assertions
        Mockito.verify(paymentTypeRepository, times(1)).saveAll(Mockito.any());
        assertEquals(paymentTypeList.size(), paymentTypeArgumentCaptor.getValue().size());
    }

    @Test
    void shouldRaiseBadRequestWithNotDeletablePaymentType() {
        List<String> paymentTypeList = getMockPaymentTypeListForCreate();
        it.pagopa.afm.marketplacebe.entity.PaymentType paymentType = TestUtil.getMockPaymentType();
        // preconditions
        when(paymentTypeRepository.findByName(anyString())).thenReturn(Optional.of(paymentType));
        when(bundleRepository.findByPaymentType(paymentType.getName())).thenReturn(List.of(any()));
        // tests and assertions
        AppException exception = assertThrows(AppException.class, () -> paymentTypeService.uploadPaymentTypeByList(paymentTypeList));
        assertEquals(HttpStatus.BAD_REQUEST, exception.getHttpStatus());
        assertEquals(PAYMENT_TYPE_NOT_DELETABLE.getTitle(), exception.getTitle());
    }
}
