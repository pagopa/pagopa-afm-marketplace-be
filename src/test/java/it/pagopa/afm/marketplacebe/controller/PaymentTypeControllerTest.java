package it.pagopa.afm.marketplacebe.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.repository.PaymentTypeRepository;
import it.pagopa.afm.marketplacebe.service.BundleService;
import it.pagopa.afm.marketplacebe.service.PaymentTypeService;

@SpringBootTest
@AutoConfigureMockMvc
class PaymentTypeControllerTest {

    public static final String URL = "/paymenttypes";

    @Autowired
    private MockMvc mvc;

    @MockBean
    private PaymentTypeRepository paymentTypeRepository;

    @MockBean
    private BundleService bundleService;

    @MockBean
    private PaymentTypeService paymentTypeService;

    @Test
    void getPaymentTypes() throws Exception {

        when(paymentTypeRepository.findAll()).thenReturn(List.of(TestUtil.getMockPaymentType()));

        mvc.perform(get(URL).contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().is2xxSuccessful());
    }

    @Test
    void getPaymentType() throws Exception {
        String url = URL + "/PO";

        when(paymentTypeRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().is2xxSuccessful());
    }

    @Test
    void createPaymentTypeByList_200() throws Exception {
        doNothing().when(paymentTypeService).syncPaymentTypes(any());

        mvc.perform(post(URL)
                        .content(TestUtil.toJson(TestUtil.getMockPaymentTypeListForCreate()))
                        .contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().isOk());
    }

    @Test
    void createPaymentTypeByList_400() throws Exception {
        AppException exception = new AppException(AppError.PAYMENT_TYPE_NOT_DELETABLE, "PaymentType");
        doThrow(exception).when(paymentTypeService).syncPaymentTypes(any());

        mvc.perform(post(URL)
                        .content(TestUtil.toJson(TestUtil.getMockPaymentTypeListForCreate()))
                        .contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

}
