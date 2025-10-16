package it.pagopa.afm.marketplacebe.service;

import static it.pagopa.afm.marketplacebe.TestUtil.getMockPaymentType;
import static it.pagopa.afm.marketplacebe.TestUtil.getMockPaymentTypeList;
import static it.pagopa.afm.marketplacebe.exception.AppError.PAYMENT_TYPE_NOT_DELETABLE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentTypeRequest;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentType;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentTypes;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.PaymentTypeRepository;

@SpringBootTest
class PaymentTypeServiceTest {

    @Captor
    ArgumentCaptor<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypeArgumentCaptor = ArgumentCaptor.forClass(it.pagopa.afm.marketplacebe.entity.PaymentType.class);

    @Captor
    ArgumentCaptor<Collection<it.pagopa.afm.marketplacebe.entity.PaymentType>> paymentTypeListArgumentCaptor = ArgumentCaptor.forClass(Collection.class);

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
    void shouldUploadPaymentTypeList_emptyInputEmptyDBNoBundle() {
        // test case: empty input list, empty element list from DB
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsInput = new ArrayList<>();
        when(paymentTypeRepository.findAll()).thenReturn(Collections.emptyList());
        // preconditions
        when(bundleRepository.findByPaymentType(anyString())).thenReturn(List.of()); // no bundle
        when(paymentTypeRepository.saveAll(Mockito.any())).thenReturn(paymentTypesAsInput);
        // tests
        executeTestForSyncPaymentTypes(paymentTypesAsInput);
    }

    @Test
    void shouldUploadPaymentTypeList_samePaymentTypesWithBundle() {
        // test case: single element (A) input list, single element (A with bundle) list from DB
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsInput = getMockPaymentTypeList();
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsDBResult = getMockPaymentTypeList();
        when(paymentTypeRepository.findAll()).thenReturn(paymentTypesAsDBResult);
        // preconditions
        when(bundleRepository.findByPaymentType(paymentTypesAsDBResult.get(0).getName())).thenReturn(List.of(TestUtil.getMockBundle())); // bundle for A
        when(paymentTypeRepository.saveAll(Mockito.any())).thenReturn(paymentTypesAsInput);
        // tests
        executeTestForSyncPaymentTypes(paymentTypesAsInput);
    }

    @Test
    void shouldUploadPaymentTypeList_differentPaymentTypesNoBundle() {
        // test case: single element (A) input list, single element (B with no bundle) list from DB
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsInput = getMockPaymentTypeList();
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsDBResult = getMockPaymentTypeList();
        paymentTypesAsDBResult.get(0).setName("PPAL");
        when(paymentTypeRepository.findAll()).thenReturn(paymentTypesAsDBResult);
        // preconditions
        when(bundleRepository.findByPaymentType(paymentTypesAsDBResult.get(0).getName())).thenReturn(List.of()); // no bundle
        when(paymentTypeRepository.saveAll(Mockito.any())).thenReturn(paymentTypesAsInput);
        // tests
        executeTestForSyncPaymentTypes(paymentTypesAsInput);
    }

    @Test
    void shouldUploadPaymentTypeList_dbSubsetOfInputWithBundle() {
        // test case: element (A, B) input list, single element (A with bundle) list from DB
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsInput = new ArrayList<>();
        paymentTypesAsInput.add(getMockPaymentType());
        paymentTypesAsInput.add(it.pagopa.afm.marketplacebe.entity.PaymentType.builder()
                .id(UUID.randomUUID().toString())
                .name("PPAL")
                .createdDate(LocalDateTime.now()).build());
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsDBResult = getMockPaymentTypeList();
        when(paymentTypeRepository.findAll()).thenReturn(paymentTypesAsDBResult);
        // preconditions
        when(bundleRepository.findByPaymentType(paymentTypesAsDBResult.get(0).getName())).thenReturn(List.of(TestUtil.getMockBundle()));
        when(paymentTypeRepository.saveAll(Mockito.any())).thenReturn(paymentTypesAsInput);
        // tests
        executeTestForSyncPaymentTypes(paymentTypesAsInput);
    }

    @Test
    void shouldUploadPaymentTypeList_dbSubsetOfInputNoBundle() {
        // test case: element (A, B) input list, single element (A with bundle) list from DB
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsInput = new ArrayList<>();
        paymentTypesAsInput.add(getMockPaymentType());
        paymentTypesAsInput.add(it.pagopa.afm.marketplacebe.entity.PaymentType.builder()
                .id(UUID.randomUUID().toString())
                .name("PPAL")
                .createdDate(LocalDateTime.now()).build());
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsDBResult = getMockPaymentTypeList();
        when(paymentTypeRepository.findAll()).thenReturn(paymentTypesAsDBResult);
        // preconditions
        when(bundleRepository.findByPaymentType(paymentTypesAsDBResult.get(0).getName())).thenReturn(List.of());
        when(paymentTypeRepository.saveAll(Mockito.any())).thenReturn(paymentTypesAsInput);
        // tests
        executeTestForSyncPaymentTypes(paymentTypesAsInput);
    }

    @Test
    void shouldUploadPaymentTypeList_inputSubsetOfDBWithBundle() {
        // test case: element (A) input list, single element (A with bundle, B) list from DB
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsInput = getMockPaymentTypeList();
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsDBResult = new ArrayList<>();
        paymentTypesAsDBResult.add(getMockPaymentType());
        paymentTypesAsDBResult.add(it.pagopa.afm.marketplacebe.entity.PaymentType.builder()
                .id(UUID.randomUUID().toString())
                .name("PPAL")
                .createdDate(LocalDateTime.now()).build());
        when(paymentTypeRepository.findAll()).thenReturn(paymentTypesAsDBResult);
        // preconditions
        when(bundleRepository.findByPaymentType(paymentTypesAsDBResult.get(0).getName())).thenReturn(List.of(TestUtil.getMockBundle()));
        when(paymentTypeRepository.saveAll(Mockito.any())).thenReturn(paymentTypesAsInput);
        // tests
        executeTestForSyncPaymentTypes(paymentTypesAsInput);
    }

    @Test
    void shouldUploadPaymentTypeList_inputSubsetOfDBNoBundle() {
        // test case: element (A) input list, single element (A with bundle, B) list from DB
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsInput = getMockPaymentTypeList();
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsDBResult = new ArrayList<>();
        paymentTypesAsDBResult.add(getMockPaymentType());
        paymentTypesAsDBResult.add(it.pagopa.afm.marketplacebe.entity.PaymentType.builder()
                .id(UUID.randomUUID().toString())
                .name("PPAL")
                .createdDate(LocalDateTime.now()).build());
        when(paymentTypeRepository.findAll()).thenReturn(paymentTypesAsDBResult);
        // preconditions
        when(bundleRepository.findByPaymentType(paymentTypesAsDBResult.get(0).getName())).thenReturn(List.of());
        when(paymentTypeRepository.saveAll(Mockito.any())).thenReturn(paymentTypesAsInput);
        // tests
        executeTestForSyncPaymentTypes(paymentTypesAsInput);
    }

    @Test
    void shouldRaiseBadRequestWithNotDeletablePaymentType() {
        // test case: element (B) input list, single element (A with bundle) list from DB
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsInput = getMockPaymentTypeList();
        paymentTypesAsInput.get(0).setName("PPAL");
        List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsDBResult = getMockPaymentTypeList();
        when(paymentTypeRepository.findAll()).thenReturn(paymentTypesAsDBResult);
        // preconditions
        when(bundleRepository.findByPaymentType(paymentTypesAsDBResult.get(0).getName())).thenReturn(List.of(TestUtil.getMockBundle()));
        when(paymentTypeRepository.saveAll(Mockito.any())).thenReturn(paymentTypesAsInput);
        // tests and assertions
        AppException exception = assertThrows(AppException.class, () -> paymentTypeService.syncPaymentTypes(paymentTypesAsInput));
        assertEquals(HttpStatus.BAD_REQUEST, exception.getHttpStatus());
        assertEquals(PAYMENT_TYPE_NOT_DELETABLE.getTitle(), exception.getTitle());
    }

    @Test
    void shouldCreatePaymentType() {
        String paymentTypeName = "CP";
        String paymentTypeDescription = "PAYMENT_TYPE_DESCRIPTION";
        PaymentTypeRequest paymentTypeRequest = PaymentTypeRequest
                .builder()
                .name(paymentTypeName)
                .description(paymentTypeDescription)
                .build();

        // Precondition
        when(paymentTypeRepository.findByName(paymentTypeName)).thenReturn(Optional.empty());
        when(paymentTypeRepository.save(any())).thenReturn(TestUtil.getMockPaymentType());

        // Tests
        it.pagopa.afm.marketplacebe.entity.PaymentType paymentType = paymentTypeService.createPaymentType(paymentTypeRequest);

        // Assertions
        assertEquals(paymentTypeName, paymentType.getName());
    }

    @Test
    void shouldCreatePaymentTypeConflict() {
        String paymentTypeName = "CP";
        String paymentTypeDescription = "PAYMENT_TYPE_DESCRIPTION";
        PaymentTypeRequest paymentTypeRequest = PaymentTypeRequest
                .builder()
                .name(paymentTypeName)
                .description(paymentTypeDescription)
                .build();

        // Precondition
        when(paymentTypeRepository.findByName(paymentTypeName)).thenReturn(Optional.of(getMockPaymentType()));

        // Test
        AppException exception = assertThrows(AppException.class, () -> {
            paymentTypeService.createPaymentType(paymentTypeRequest);
        });

        assertEquals(HttpStatus.CONFLICT, exception.getHttpStatus());
    }

    @Test
    void shouldDeletePaymentType() {
        it.pagopa.afm.marketplacebe.entity.PaymentType paymentType = TestUtil.getMockPaymentType();

        // Precondition
        when(paymentTypeRepository.findByName(anyString())).thenReturn(Optional.of(paymentType));
        when(bundleRepository.findByPaymentType(anyString())).thenReturn(new ArrayList<>());

        // Tests
        paymentTypeService.deletePaymentType(paymentType.getName());

        verify(paymentTypeRepository).delete(paymentTypeArgumentCaptor.capture());

        assertEquals(paymentType.getName(), paymentTypeArgumentCaptor.getValue().getName());
    }

    @Test
    void shouldThrowNotFoundDeletePaymentType() {
        it.pagopa.afm.marketplacebe.entity.PaymentType paymentType = TestUtil.getMockPaymentType();
        String paymentTypeName = paymentType.getName();
        // Precondition
        when(paymentTypeRepository.findByName(anyString())).thenReturn(Optional.empty());

        // Tests
        AppException exception = assertThrows(AppException.class, () -> {
            paymentTypeService.deletePaymentType(paymentTypeName);
        });

        assertEquals(HttpStatus.NOT_FOUND, exception.getHttpStatus());
    }

    @Test
    void shouldThrowBadRequestDeletePaymentType() {
        it.pagopa.afm.marketplacebe.entity.PaymentType paymentType = TestUtil.getMockPaymentType();
        String paymentTypeName = paymentType.getName();

        // Precondition
        when(paymentTypeRepository.findByName(anyString())).thenReturn(Optional.of(paymentType));
        when(bundleRepository.findByPaymentType(anyString())).thenReturn(List.of(TestUtil.getMockBundle()));

        // Tests
        AppException exception = assertThrows(AppException.class, () -> {
            paymentTypeService.deletePaymentType(paymentTypeName);
        });

        assertEquals(HttpStatus.BAD_REQUEST, exception.getHttpStatus());
    }

    void executeTestForSyncPaymentTypes(List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypesAsInput) {
        paymentTypeService.syncPaymentTypes(paymentTypesAsInput);
        verify(paymentTypeRepository).saveAll(paymentTypeListArgumentCaptor.capture());
        Mockito.verify(paymentTypeRepository, times(1)).saveAll(Mockito.any());
        assertEquals(paymentTypesAsInput.size(), paymentTypeListArgumentCaptor.getValue().size());
    }
}
