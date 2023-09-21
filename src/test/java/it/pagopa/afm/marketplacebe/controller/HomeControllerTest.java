package it.pagopa.afm.marketplacebe.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.azure.spring.data.cosmos.core.CosmosTemplate;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.service.BundleService;

@SpringBootTest
@AutoConfigureMockMvc
class HomeControllerTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private BundleService bundleService;
    
    @MockBean 
    private CosmosTemplate cosmosTemplate;

    @Test
    void getHome() throws Exception {
        String url = "/";
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is3xxRedirection());
    }

    @Test
    void getHealthCheck() throws Exception {
        String url = "/info";
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getGlobalBundles() throws Exception {
        when(bundleService.getBundles(anyList(), any(), anyInt(), anyInt())).thenReturn(TestUtil.getMockBundles());

        String url = "/bundles";
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }
    
    @Test
    void getGlobalBundlesByName() throws Exception {
        when(bundleService.getBundles(anyList(), any(), anyInt(), anyInt())).thenReturn(TestUtil.getMockBundles());

        String url = "/bundles";
        mvc.perform(get(url).param("name","mockName").contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getConfiguration() throws Exception {
        String url = "/configuration";
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

}
