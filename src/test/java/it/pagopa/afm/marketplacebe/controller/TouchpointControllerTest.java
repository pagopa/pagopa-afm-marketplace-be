package it.pagopa.afm.marketplacebe.controller;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.model.CalculatorConfiguration;
import it.pagopa.afm.marketplacebe.repository.TouchpointRepository;
import it.pagopa.afm.marketplacebe.service.BundleService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Optional;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
class TouchpointControllerTest {


    @Autowired
    private MockMvc mvc;

    @MockBean
    private TouchpointRepository touchpointRepository;

    @MockBean
    private BundleService bundleService;

    @Test
    void getTouchpoints() throws Exception {
        String url = "/touchpoints";

        when(touchpointRepository.findAll()).thenReturn(TestUtil.getMockTouchpoints());

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().is2xxSuccessful());
    }

    @Test
    void getTouchpoint() throws Exception {
        String url = "/touchpoints/"+ UUID.randomUUID();

        when(touchpointRepository.findById(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().is2xxSuccessful());
    }

    @Test
    void createTouchpoint() throws Exception {
        String url = "/touchpoints";

        when(touchpointRepository.save(any())).thenReturn(TestUtil.getMockTouchpoint());
        when(touchpointRepository.findByName(anyString())).thenReturn(Optional.empty());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockTouchpointRequest()))
                        .contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().is2xxSuccessful());
    }

    @Test
    void deteteTouchpoint() throws Exception {
        String url = "/touchpoints/"+ UUID.randomUUID();

        when(touchpointRepository.findById(anyString())).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        mvc.perform(delete(url).contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().is2xxSuccessful());
    }

}
