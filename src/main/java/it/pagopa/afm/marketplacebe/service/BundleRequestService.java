package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundleRequest;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.entity.*;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.request.BundleRequestId;
import it.pagopa.afm.marketplacebe.model.request.CiBundleRequest;
import it.pagopa.afm.marketplacebe.model.request.CiBundleSubscriptionRequest;
import it.pagopa.afm.marketplacebe.model.request.CiRequests;
import it.pagopa.afm.marketplacebe.model.request.PspBundleRequest;
import it.pagopa.afm.marketplacebe.model.request.PspRequests;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@Slf4j
public class BundleRequestService {

    @Autowired
    BundleRepository bundleRepository;

    @Autowired
    BundleRequestRepository bundleRequestRepository;

    @Autowired
    ArchivedBundleRequestRepository archivedBundleRequestRepository;

    @Autowired
    CiBundleRepository ciBundleRepository;

    @Autowired
    ModelMapper modelMapper;

    /**
     * Get requests for creditor institution
     *
     * @param ciFiscalCode
     * @param size
     * @param cursor
     * @param idPsp        optional
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

        if (bundle.getValidityDateTo() != null) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, "Bundle has been deleted.");
        }

        if (!bundle.getType().equals(BundleType.PUBLIC)) {
            throw new AppException(AppError.BUNDLE_REQUEST_BAD_REQUEST, idBundle, "Type not public");
        }

        List<CiBundleAttribute> attributes = (ciBundleSubscriptionRequest.getCiBundleAttributeModelList() != null
                && !ciBundleSubscriptionRequest.getCiBundleAttributeModelList().isEmpty()) ?
                ciBundleSubscriptionRequest.getCiBundleAttributeModelList()
                        .stream()
                        .map(attribute ->
                                CiBundleAttribute.builder()
                                        .id(idBundle + "-" + UUID.randomUUID())
                                        .insertedDate(LocalDateTime.now())
                                        .maxPaymentAmount(attribute.getMaxPaymentAmount())
                                        .transferCategory(attribute.getTransferCategory())
                                        .transferCategoryRelation(attribute.getTransferCategoryRelation())
                                        .build()
                        ).collect(Collectors.toList()) : new ArrayList<>();

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
        // archive the bundle request
        // remove bundle request

        Optional<BundleRequest> bundleRequest = bundleRequestRepository.findById(idBundleRequest);

        if (bundleRequest.isEmpty()) {
            throw new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, idBundleRequest);
        }

        if (!bundleRequest.get().getCiFiscalCode().equals(ciFiscalCode)) {
            throw new AppException(AppError.BUNDLE_REQUEST_BAD_REQUEST, idBundleRequest, String.format("ciFiscalCode=%s", ciFiscalCode));
        }

        archiveBundleRequest(bundleRequest.get(), null);
    }


    public PspRequests getRequestsByPsp(String idPsp, Integer limit, Integer pageNumber, String cursor, @Nullable String ciFiscalCode) {
        // TODO: pageable

        List<BundleRequest> result;
        if (ciFiscalCode != null) {
            result = bundleRequestRepository.findByCiFiscalCodeAndIdPsp(ciFiscalCode, idPsp);
        } else {
            result = bundleRequestRepository.findByIdPsp(idPsp);
        }
        return PspRequests.builder()
                .requestsList(result.stream()
                        .filter(Objects::nonNull)
                        .map(elem -> modelMapper.map(elem, PspBundleRequest.class))
                        .collect(Collectors.toList()))
                .build();
    }


    public void acceptRequest(String idPsp, String idBundleRequest) {
        var bundleRequest = getBundleRequest(idPsp, idBundleRequest);
        if (bundleRequest.getAcceptedDate() == null && bundleRequest.getRejectionDate() == null) {
            // archive the bundle request
            archiveBundleRequest(bundleRequest, true);

            // verify if it is a new relation or should be updated an existent relationship
            createCiBundleRelation(bundleRequest);
        } else if (bundleRequest.getAcceptedDate() == null && bundleRequest.getRejectionDate() != null) {
            throw new AppException(AppError.REQUEST_ALREADY_REJECTED, idBundleRequest, bundleRequest.getRejectionDate());
        } else {
            throw new AppException(AppError.REQUEST_ALREADY_ACCEPTED, idBundleRequest, bundleRequest.getAcceptedDate());
        }
    }


    public void rejectRequest(String idPsp, String idBundleRequest) {
        var bundleRequest = getBundleRequest(idPsp, idBundleRequest);
        if (bundleRequest.getRejectionDate() == null && bundleRequest.getAcceptedDate() == null) {
            // archive the bundle request
            archiveBundleRequest(bundleRequest, false);

        } else if (bundleRequest.getAcceptedDate() != null && bundleRequest.getRejectionDate() == null) {
            throw new AppException(AppError.REQUEST_ALREADY_ACCEPTED, idBundleRequest, bundleRequest.getAcceptedDate());
        } else {
            throw new AppException(AppError.REQUEST_ALREADY_REJECTED, idBundleRequest, bundleRequest.getAcceptedDate());
        }
    }

    /**
     * verifies if it is a new relation or should be updated an existent relationship
     *
     * @param bundleRequest entity of {@link BundleRequest}
     */
    private void createCiBundleRelation(BundleRequest bundleRequest) {
        Optional<CiBundle> optCiBundle = ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundleRequest.getIdBundle(), bundleRequest.getCiFiscalCode());
        if (optCiBundle.isEmpty()) {
            ciBundleRepository.save(buildCiBundle(bundleRequest));
        } else {
            CiBundle ciBundle = optCiBundle.get();
            ciBundle.setAttributes(bundleRequest.getCiBundleAttributes());
            ciBundleRepository.save(ciBundle);
        }
    }

    /**
     * @param idPsp           PSP identifier
     * @param idBundleRequest Bundle Request identifier
     * @return the entity if exist
     * @throws AppException if not found
     */
    private BundleRequest getBundleRequest(String idPsp, String idBundleRequest) {
        return bundleRequestRepository.findByIdAndIdPsp(idBundleRequest, idPsp)
                .orElseThrow(() -> new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, idBundleRequest));
    }

    private CiBundle buildCiBundle(BundleRequest entity) {
        return CiBundle.builder()
                .ciFiscalCode(entity.getCiFiscalCode())
                .idBundle(entity.getIdBundle())
                .attributes(entity.getCiBundleAttributes())
//                .validityDateFrom()
                .build();
    }

    /**
     * Archives a bundle request.
     * Create a new {@link ArchivedBundleRequest} entity and deletes the {@link BundleRequest} entity.
     *
     * @param bundleRequest an entity of {@link BundleRequest}
     * @param accepted      true = request is accepted, false = not accepted, null = deleted
     */
    private void archiveBundleRequest(BundleRequest bundleRequest, Boolean accepted) {
        var requestToArchive = bundleRequest;
        if (Boolean.TRUE.equals(accepted)) {
            requestToArchive = requestToArchive.toBuilder()
                    .acceptedDate(LocalDateTime.now())
                    .build();
        }
        if (Boolean.FALSE.equals(accepted)) {
            requestToArchive = requestToArchive.toBuilder()
                    .rejectionDate(LocalDateTime.now())
                    .build();
        }

        archivedBundleRequestRepository.save(modelMapper.map(requestToArchive, ArchivedBundleRequest.class));
        bundleRequestRepository.delete(bundleRequest);
    }
}
