package it.pagopa.afm.marketplacebe.controller;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.service.BundleService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
class PspControllerTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private BundleService bundleService;

    private final String BUNDLES = "/psps/%s/bundles";
    private final String BUNDLE = BUNDLES + "/%s";

    @Test
    void getBundles_200() throws Exception {
        when(bundleService.getBundlesByIdPsp(anyString(), anyInt(), anyInt())).thenReturn(TestUtil.getMockBundles());

        String url = String.format(BUNDLES, TestUtil.getMockIdPsp());
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getBundles_404() throws Exception {
        when(bundleService.getBundlesByIdPsp(anyString(), anyInt(), anyInt())).thenReturn(TestUtil.getMockBundles());

        String url = String.format(BUNDLES, "");
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    void getBundle_200() throws Exception {
        when(bundleService.getBundleById(anyString(), anyString())).thenReturn(TestUtil.getMockPspBundleDetails());

        String url = String.format(BUNDLE, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getBundle_404() throws Exception {
        String idBundle = TestUtil.getMockIdBundle();
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        doThrow(exception).when(bundleService).getBundleById(anyString(), anyString());

        String url = String.format(BUNDLE, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

}
