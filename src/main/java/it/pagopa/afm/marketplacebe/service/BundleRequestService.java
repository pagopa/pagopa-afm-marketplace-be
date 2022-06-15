package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.request.*;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class BundleRequestService {

    @Autowired
    BundleRepository bundleRepository;

    @Autowired
    BundleRequestRepository bundleRequestRepository;

    @Autowired
    ModelMapper modelMapper;

    /**
     * Get requests for creditor institution
     * @param ciFiscalCode
     * @param size
     * @param cursor
     * @param idPsp optional
     * @return
     */
    public CiRequests getRequestsByCI(String ciFiscalCode, Integer size, String cursor, String idPsp) {
        // TODO: pageable

        List<BundleRequest> requests = (idPsp == null) ?
                bundleRequestRepository.findByCiFiscalCode(ciFiscalCode) :
                bundleRequestRepository.findByCiFiscalCodeAndIdPsp(ciFiscalCode, idPsp);

        List<CiBundleRequest> ciBundleRequestList = requests.stream()
                .map(request -> modelMapper.map(request, CiBundleRequest.class)).collect(Collectors.toList());

        return CiRequests.builder()
                .requestsList(ciBundleRequestList)
                .build();
    }

    public BundleRequestId createBundleRequest(String ciFiscalCode, CiBundleSubscriptionRequest ciBundleSubscriptionRequest) {
        // retrieve bundle by idBundle and check if it is public

        String idBundle = ciBundleSubscriptionRequest.getIdBundle();
        Optional<Bundle> optBundle = bundleRepository.findById(idBundle);

        if (optBundle.isEmpty()) {
            throw new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        }

        Bundle bundle = optBundle.get();

        if (!bundle.getType().equals(BundleType.PUBLIC)) {
            throw new AppException(AppError.BUNDLE_REQUEST_BAD_REQUEST, idBundle, "type not public");
        }

        List<CiBundleAttribute> attributes = ciBundleSubscriptionRequest.getCiBundleAttributeList()
                .stream()
                .map(attribute ->
                        CiBundleAttribute.builder()
                                .insertedDate(LocalDateTime.now())
                                .maxPaymentAmount(attribute.getMaxPaymentAmount())
                                .transferCategory(attribute.getTransferCategory())
                                .transferCategoryRelation(attribute.getTransferCategoryRelation())
                                .build()
                ).collect(Collectors.toList());

        BundleRequest request = BundleRequest.builder()
                .idBundle(bundle.getId())
                .idPsp(bundle.getIdPsp())
                .ciFiscalCode(ciFiscalCode)
                .ciBundleAttributes(attributes)
                .build();

        bundleRequestRepository.save(request);

        return BundleRequestId.builder().idBundleRequest(request.getId()).build();
    }

    public void removeBundleRequest(String ciFiscalCode, String idBundleRequest) {
        // find bundle request by id
        // check the related ciFiscalCode
        // check removal validity
        // remove bundle request

        Optional<BundleRequest> bundleRequest = bundleRequestRepository.findById(idBundleRequest);

        if (bundleRequest.isEmpty()) {
            throw new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, idBundleRequest);
        }

        if (!bundleRequest.get().getCiFiscalCode().equals(ciFiscalCode)) {
            throw new AppException(AppError.BUNDLE_REQUEST_BAD_REQUEST, idBundleRequest, String.format("ciFiscalCode=%s", ciFiscalCode));
        }

        if (bundleRequest.get().getAcceptedDate() != null || bundleRequest.get().getRejectionDate() != null) {
            throw new AppException(AppError.BUNDLE_REQUEST_CONFLICT, idBundleRequest, "Request already elaborated by PSP");
        }

        bundleRequestRepository.delete(bundleRequest.get());
    }
}
