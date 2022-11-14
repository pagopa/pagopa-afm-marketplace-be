package it.pagopa.afm.marketplacebe.controller;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.service.BundleOfferService;
import it.pagopa.afm.marketplacebe.service.BundleRequestService;
import it.pagopa.afm.marketplacebe.service.BundleService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
class CiControllerTest {

    private final String BUNDLES = "/cis/%s/bundles";
    private final String BUNDLE = BUNDLES + "/%s";
    private final String BUNDLE_ATTRIBUTES = BUNDLE + "/attributes";
    private final String REQUESTS = "/cis/%s/requests";
    private final String OFFERS = "/cis/%s/offers";
    private final String ACCEPT_OFFER = OFFERS + "/%s/accept";
    private final String REJECT_OFFER = OFFERS + "/%s/reject";
    @Autowired
    private MockMvc mvc;
    @MockBean
    private BundleService bundleService;
    @MockBean
    private BundleOfferService bundleOfferService;
    @MockBean
    private BundleRequestService bundleRequestService;

    @Test
    void getBundlesByFiscalCode_200() throws Exception {
        when(bundleService.getBundlesByFiscalCode(anyString(), anyInt(), anyInt())).thenReturn(TestUtil.getMockCiBundles());

        String url = String.format(BUNDLES, TestUtil.getMockCiFiscalCode());
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getBundles_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleService).getBundlesByFiscalCode(anyString(), anyInt(), anyInt());

        when(bundleService.getBundlesByIdPsp(anyString(), anyInt(), anyInt())).thenReturn(TestUtil.getMockBundles());

        String url = String.format(BUNDLES, TestUtil.getMockCiFiscalCode());
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    void getBundleByFiscalCode_200() throws Exception {
        when(bundleService.getBundleByFiscalCode(anyString(), anyString())).thenReturn(TestUtil.getMockBundleDetailsForCi());

        String url = String.format(BUNDLE, TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle());
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getBundleByFiscalCode_404() throws Exception {
        String idBundle = TestUtil.getMockIdBundle();
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        doThrow(exception).when(bundleService).getBundleByFiscalCode(anyString(), anyString());

        String url = String.format(BUNDLE, TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle());
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void removeBundleByFiscalCode_200() throws Exception {
        String url = String.format(BUNDLE, TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle());
        mvc.perform(delete(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void removeBundleByFiscalCode_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleService).removeBundleByFiscalCode(anyString(), anyString());

        String url = String.format(BUNDLE, TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle());
        mvc.perform(delete(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    void getBundleAttributesByFiscalCode_200() throws Exception {
        when(bundleService.getBundleAttributesByFiscalCode(anyString(), anyString())).thenReturn(TestUtil.getMockBundleDetailsAttributes());

        String url = String.format(BUNDLE_ATTRIBUTES, TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode());

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void createBundleAttributesByCi_201() throws Exception {
        when(bundleService.createBundleAttributesByCi(anyString(), anyString(), any())).thenReturn(TestUtil.getMockBundleAttributeResponse());

        String url = String.format(BUNDLE_ATTRIBUTES, TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiBundleAttributeModel()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isCreated())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void createBundleAttributesByCi_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_BAD_REQUEST, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleService).createBundleAttributesByCi(anyString(), anyString(), any());

        String url = String.format(BUNDLE_ATTRIBUTES, TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiBundleAttributeModel()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void createBundleAttributesByCi_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleService).createBundleAttributesByCi(anyString(), anyString(), any());

        String url = String.format(BUNDLE_ATTRIBUTES, TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiBundleAttributeModel()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void updateBundleAttributesByCi_200() throws Exception {
        String url = String.format(BUNDLE_ATTRIBUTES + "/%s", TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode(), "idAttribute");

        mvc.perform(put(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiBundleAttributeModel()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void updateBundleAttributesByCi_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_BAD_REQUEST, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleService).updateBundleAttributesByCi(anyString(), anyString(), anyString(), any());

        String url = String.format(BUNDLE_ATTRIBUTES + "/%s", TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode(), "idAttribute");

        mvc.perform(put(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiBundleAttributeModel()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void updateBundleAttributesByCi_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleService).updateBundleAttributesByCi(anyString(), anyString(), anyString(), any());

        String url = String.format(BUNDLE_ATTRIBUTES + "/%s", TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode(), "idAttribute");

        mvc.perform(put(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiBundleAttributeModel()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void removeBundleAttributesByCi_200() throws Exception {
        String url = String.format(BUNDLE_ATTRIBUTES + "/%s", TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode(), "idAttribute");

        mvc.perform(delete(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void removeBundleAttributesByCi_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_BAD_REQUEST, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleService).removeBundleAttributesByCi(anyString(), anyString(), anyString());

        String url = String.format(BUNDLE_ATTRIBUTES + "/%s", TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode(), "idAttribute");

        mvc.perform(delete(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void removeBundleAttributesByCi_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleService).removeBundleAttributesByCi(anyString(), anyString(), anyString());

        String url = String.format(BUNDLE_ATTRIBUTES + "/%s", TestUtil.getMockCiFiscalCode(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode(), "idAttribute");

        mvc.perform(delete(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getRequestsByCI_200() throws Exception {
        when(bundleRequestService.getRequestsByCI(anyString(), anyInt(), anyString(), anyString())).thenReturn(TestUtil.getMockCiRequests());

        String url = String.format(REQUESTS, TestUtil.getMockCiFiscalCode());

        mvc.perform(get(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void createRequest_201() throws Exception {
        when(bundleRequestService.createBundleRequest(anyString(), any())).thenReturn(TestUtil.getMockBundleRequestId());

        String url = String.format(REQUESTS, TestUtil.getMockCiFiscalCode());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiBundleSubscriptionRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isCreated())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void createRequest_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_BAD_REQUEST, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleRequestService).createBundleRequest(anyString(), any());

        String url = String.format(REQUESTS, TestUtil.getMockCiFiscalCode());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiBundleSubscriptionRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void createRequest_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleRequestService).createBundleRequest(anyString(), any());

        String url = String.format(REQUESTS, TestUtil.getMockCiFiscalCode());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiBundleSubscriptionRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void removeRequest_200() throws Exception {
        String url = String.format(REQUESTS + "/%s", TestUtil.getMockCiFiscalCode(), "idBundleRequest");

        mvc.perform(delete(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void removeRequest_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_BAD_REQUEST, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleRequestService).removeBundleRequest(anyString(), anyString());

        String url = String.format(REQUESTS + "/%s", TestUtil.getMockCiFiscalCode(), "idBundleRequest");

        mvc.perform(delete(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void removeRequest_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleRequestService).removeBundleRequest(anyString(), anyString());

        String url = String.format(REQUESTS + "/%s", TestUtil.getMockCiFiscalCode(), "idBundleRequest");

        mvc.perform(delete(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getOffersByCI_200() throws Exception {
        when(bundleOfferService.getCiOffers(anyString(), anyString())).thenReturn(TestUtil.getMockBundleCiOffers());

        String url = String.format(OFFERS, TestUtil.getMockCiFiscalCode());

        mvc.perform(get(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void acceptOffer_201() throws Exception {
        when(bundleOfferService.acceptOffer(anyString(), anyString())).thenReturn(TestUtil.getMockCiBundleId());

        String url = String.format(ACCEPT_OFFER, TestUtil.getMockCiFiscalCode(), "idBundleOffer");

        mvc.perform(post(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isCreated());
    }

    @Test
    void acceptOffer_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleOfferService).acceptOffer(anyString(), anyString());

        String url = String.format(ACCEPT_OFFER, TestUtil.getMockCiFiscalCode(), "idBundleOffer");

        mvc.perform(post(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    void acceptOffer_409() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED, TestUtil.getMockIdBundle(), "");
        doThrow(exception).when(bundleOfferService).acceptOffer(anyString(), anyString());

        String url = String.format(ACCEPT_OFFER, TestUtil.getMockCiFiscalCode(), "idBundleOffer");

        mvc.perform(post(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isConflict());
    }

    @Test
    void rejectOffer_200() throws Exception {
        String url = String.format(REJECT_OFFER, TestUtil.getMockCiFiscalCode(), "idBundleOffer");

        mvc.perform(post(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void rejectOffer_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, TestUtil.getMockIdBundle());
        doThrow(exception).when(bundleOfferService).rejectOffer(anyString(), anyString());

        String url = String.format(REJECT_OFFER, TestUtil.getMockCiFiscalCode(), "idBundleOffer");

        mvc.perform(post(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    void rejectOffer_409() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_OFFER_ALREADY_REJECTED, TestUtil.getMockIdBundle(), "");
        doThrow(exception).when(bundleOfferService).rejectOffer(anyString(), anyString());

        String url = String.format(REJECT_OFFER, TestUtil.getMockCiFiscalCode(), "idBundleOffer");

        mvc.perform(post(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isConflict());
    }

}
