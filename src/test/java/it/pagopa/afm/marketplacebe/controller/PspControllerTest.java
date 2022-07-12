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

import java.util.List;

import static org.mockito.ArgumentMatchers.*;
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
class PspControllerTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private BundleService bundleService;

    @MockBean
    private BundleOfferService bundleOfferService;

    @MockBean
    private BundleRequestService bundleRequestService;

    private final String BUNDLES = "/psps/%s/bundles";
    private final String BUNDLE = BUNDLES + "/%s";
    private final String CI_BUNDLES = BUNDLE + "/creditorInstitutions";
    private final String OFFERS = "/psps/%s/offers";
    private final String SEND_OFFER = BUNDLE + "/offers";
    private final String REQUESTS = "/psps/%s/requests";
    private final String ACCEPT_REQUEST = REQUESTS + "/%s/accept";
    private final String REJECT_REQUEST = REQUESTS + "/%s/reject";

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

    @Test
    void getBundleCreditorInstitutions_200() throws Exception {
        when(bundleService.getCIs(anyString(), anyString())).thenReturn(TestUtil.getMockCiFiscalCodeList());

        String url = String.format(CI_BUNDLES, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getBundleCreditorInstitutions_404() throws Exception {
        String idBundle = TestUtil.getMockIdBundle();
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        doThrow(exception).when(bundleService).getCIs(anyString(), anyString());

        String url = String.format(CI_BUNDLES, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());
        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getBundleCreditorInstitutionDetails_200() throws Exception {
        when(bundleService.getCIDetails(anyString(), anyString(), anyString())).thenReturn(TestUtil.getMockCiBundleDetails());

        String url = String.format(CI_BUNDLES + "/%s", TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode());

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getBundleCreditorInstitutionDetails_404() throws Exception {
        String idBundle = TestUtil.getMockIdBundle();
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        doThrow(exception).when(bundleService).getCIDetails(anyString(), anyString(), anyString());

        String url = String.format(CI_BUNDLES + "/%s", TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle(), TestUtil.getMockCiFiscalCode());

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void createBundle_201() throws Exception {
        when(bundleService.createBundle(anyString(), any())).thenReturn(TestUtil.getMockBundleResponse());

        String url = String.format(BUNDLES, TestUtil.getMockIdPsp());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockBundleRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isCreated());
    }

    @Test
    void createBundle_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDate");
        doThrow(exception).when(bundleService).createBundle(anyString(), any());

        String url = String.format(BUNDLES, TestUtil.getMockIdPsp());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockBundleRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void createBundle_409() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NAME_CONFLICT, "Name");
        doThrow(exception).when(bundleService).createBundle(anyString(), any());

        String url = String.format(BUNDLES, TestUtil.getMockIdPsp());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockBundleRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isConflict())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void updateBundle_200() throws Exception {
        when(bundleService.updateBundle(anyString(), anyString(), any())).thenReturn(TestUtil.getMockBundle());

        String url = String.format(BUNDLE, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(put(url)
                        .content(TestUtil.toJson(TestUtil.getMockBundleRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void updateBundle_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDate");
        doThrow(exception).when(bundleService).updateBundle(anyString(), anyString(), any());

        String url = String.format(BUNDLE, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(put(url)
                        .content(TestUtil.toJson(TestUtil.getMockBundleRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void updateBundle_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, "idbundle");
        doThrow(exception).when(bundleService).updateBundle(anyString(), anyString(), any());

        String url = String.format(BUNDLE, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(put(url)
                        .content(TestUtil.toJson(TestUtil.getMockBundleRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void updateBundle_409() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NAME_CONFLICT, "name");
        doThrow(exception).when(bundleService).updateBundle(anyString(), anyString(), any());

        String url = String.format(BUNDLE, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(put(url)
                        .content(TestUtil.toJson(TestUtil.getMockBundleRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isConflict())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void removeBundle_200() throws Exception {
        String url = String.format(BUNDLE, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(delete(url)
                        .content(TestUtil.toJson(TestUtil.getMockBundleRequest()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void removeBundle_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_BAD_REQUEST, "idbundle");
        doThrow(exception).when(bundleService).removeBundle(anyString(), anyString());

        String url = String.format(BUNDLE, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(delete(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void removeBundle_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, "idbundle");
        doThrow(exception).when(bundleService).removeBundle(anyString(), anyString());

        String url = String.format(BUNDLE, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(delete(url)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getOffers_200() throws Exception {
        when(bundleOfferService.getPspOffers(anyString())).thenReturn(TestUtil.getMockBundleOffers());

        String url = String.format(OFFERS, TestUtil.getMockIdPsp());

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void sendBundleOffer_201() throws Exception {
        when(bundleOfferService.sendBundleOffer(anyString(), anyString(), any())).thenReturn(List.of(TestUtil.getMockBundleOffered()));

        String url = String.format(SEND_OFFER, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiFiscalCodeList()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isCreated())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }


    @Test
    void sendBundleOffer_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_BAD_REQUEST, "idbundle");
        doThrow(exception).when(bundleOfferService).sendBundleOffer(anyString(), anyString(), any());

        String url = String.format(SEND_OFFER, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiFiscalCodeList()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void sendBundleOffer_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_NOT_FOUND, "idbundle");
        doThrow(exception).when(bundleOfferService).sendBundleOffer(anyString(), anyString(), any());

        String url = String.format(SEND_OFFER, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiFiscalCodeList()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void sendBundleOffer_409() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_OFFER_CONFLICT, "idbundle", "name");
        doThrow(exception).when(bundleOfferService).sendBundleOffer(anyString(), anyString(), any());

        String url = String.format(SEND_OFFER, TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle());

        mvc.perform(post(url)
                        .content(TestUtil.toJson(TestUtil.getMockCiFiscalCodeList()))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isConflict())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void removeBundleOffer_200() throws Exception {
        String url = String.format(SEND_OFFER + "/%s", TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle(), "idBundleOffer");

        mvc.perform(delete(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void removeBundleOffer_400() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_OFFER_BAD_REQUEST, "idbundle", "name");
        doThrow(exception).when(bundleOfferService).removeBundleOffer(anyString(), anyString(), anyString());

        String url = String.format(SEND_OFFER + "/%s", TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle(), "idBundleOffer");

        mvc.perform(delete(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void removeBundleOffer_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_OFFER_NOT_FOUND, "idbundle", "name");
        doThrow(exception).when(bundleOfferService).removeBundleOffer(anyString(), anyString(), anyString());

        String url = String.format(SEND_OFFER + "/%s", TestUtil.getMockIdPsp(), TestUtil.getMockIdBundle(), "idBundleOffer");

        mvc.perform(delete(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void getRequestsByPsp_200() throws Exception {
        when(bundleRequestService.getRequestsByPsp(anyString(), anyInt(), anyInt(), anyString(), anyString())).thenReturn(TestUtil.getMockPspRequests());

        String url = String.format(REQUESTS, TestUtil.getMockIdPsp());

        mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void acceptRequest_201() throws Exception {
        String url = String.format(ACCEPT_REQUEST, TestUtil.getMockIdPsp(), "idBundleRequest");

        mvc.perform(post(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isCreated());
    }

    @Test
    void acceptRequest_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, "idbundle", "name");
        doThrow(exception).when(bundleRequestService).acceptRequest(anyString(), anyString());

        String url = String.format(ACCEPT_REQUEST, TestUtil.getMockIdPsp(), "idBundleRequest");

        mvc.perform(post(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void acceptRequest_409() throws Exception {
        AppException exception = new AppException(AppError.REQUEST_ALREADY_ACCEPTED, "idbundle", "name");
        doThrow(exception).when(bundleRequestService).acceptRequest(anyString(), anyString());

        String url = String.format(ACCEPT_REQUEST, TestUtil.getMockIdPsp(), "idBundleRequest");

        mvc.perform(post(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isConflict())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void rejectRequest_200() throws Exception {
        String url = String.format(REJECT_REQUEST, TestUtil.getMockIdPsp(), "idBundleRequest");

        mvc.perform(post(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void rejectRequest_404() throws Exception {
        AppException exception = new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, "idbundle", "name");
        doThrow(exception).when(bundleRequestService).rejectRequest(anyString(), anyString());

        String url = String.format(REJECT_REQUEST, TestUtil.getMockIdPsp(), "idBundleRequest");

        mvc.perform(post(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    void rejectRequest_409() throws Exception {
        AppException exception = new AppException(AppError.REQUEST_ALREADY_REJECTED, "idbundle", "name");
        doThrow(exception).when(bundleRequestService).rejectRequest(anyString(), anyString());

        String url = String.format(REJECT_REQUEST, TestUtil.getMockIdPsp(), "idBundleRequest");

        mvc.perform(post(url).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isConflict())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

}
