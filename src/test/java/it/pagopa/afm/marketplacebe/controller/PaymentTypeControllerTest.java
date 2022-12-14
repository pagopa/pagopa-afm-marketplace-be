package it.pagopa.afm.marketplacebe.controller;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.repository.PaymentTypeRepository;
import it.pagopa.afm.marketplacebe.repository.TouchpointRepository;
import it.pagopa.afm.marketplacebe.service.BundleService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
class PaymentTypeControllerTest {


    @Autowired
    private MockMvc mvc;

    @MockBean
    private PaymentTypeRepository paymentTypeRepository;

    @MockBean
    private BundleService bundleService;

    @Test
    void getPaymentTypes() throws Exception {
        String url = "/paymenttypes";

        when(paymentTypeRepository.findAll()).thenReturn(List.of(TestUtil.getMockPaymentType()));

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().is2xxSuccessful());
    }

    @Test
    void getPaymentType() throws Exception {
        String url = "/paymenttypes/PO";

        when(paymentTypeRepository.findByName(anyString())).thenReturn(Optional.of(TestUtil.getMockPaymentType()));

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().is2xxSuccessful());
    }

}
