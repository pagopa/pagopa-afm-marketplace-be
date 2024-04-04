package it.pagopa.afm.marketplacebe.service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundleRequest;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
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
import it.pagopa.afm.marketplacebe.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BundleRequestService {

    public static final String ALREADY_DELETED = "Bundle has been deleted.";

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
        List<BundleRequestEntity> requests = (idPsp == null) ?
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

        Bundle bundle = getBundle(idBundle);

        // a bundle request is acceptable if validityDateTo is after now
        if (!CommonUtil.isValidityDateToAcceptable(bundle.getValidityDateTo())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        if (!bundle.getType().equals(BundleType.PUBLIC)) {
            throw new AppException(AppError.BUNDLE_OFFER_CONFLICT, idBundle, "Type not public");
        }

        // rule R15: attribute payment amount should be lower than bundle one
        if (ciBundleSubscriptionRequest.getCiBundleAttributeModelList() != null) {
            ciBundleSubscriptionRequest.getCiBundleAttributeModelList().parallelStream().forEach(attribute -> {
                if (attribute.getMaxPaymentAmount().compareTo(bundle.getPaymentAmount()) > 0) {
                    throw new AppException(AppError.BUNDLE_REQUEST_BAD_REQUEST, idBundle, "Payment amount should be lower than or equal to bundle payment amount.");
                }
            });
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

        BundleRequestEntity request = BundleRequestEntity.builder()
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

        Optional<BundleRequestEntity> bundleRequest = bundleRequestRepository.findById(idBundleRequest);

        if (bundleRequest.isEmpty()) {
            throw new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, idBundleRequest);
        }

        if (!bundleRequest.get().getCiFiscalCode().equals(ciFiscalCode)) {
            throw new AppException(AppError.BUNDLE_REQUEST_BAD_REQUEST, idBundleRequest, String.format("ciFiscalCode=%s", ciFiscalCode));
        }

        archiveBundleRequest(bundleRequest.get(), null);
    }


    public PspRequests getRequestsByPsp(String idPsp, Integer limit, Integer pageNumber, @Nullable String ciFiscalCode, @Nullable String idBundle) {
        List<BundleRequestEntity> result = bundleRequestRepository.findByIdPspAndFiscalCodeAndType(idPsp, ciFiscalCode, idBundle, limit * pageNumber, limit);

        return PspRequests.builder()
                .requestsList(result.stream()
                        .filter(Objects::nonNull)
                        .map(elem -> modelMapper.map(elem, PspBundleRequest.class))
                        .collect(Collectors.toList()))
                .pageInfo(PageInfo.builder()
                        .page(pageNumber)
                        .limit(limit)
                        .build())
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
     * @param bundleRequestEntity entity of {@link BundleRequestEntity}
     */
    private void createCiBundleRelation(BundleRequestEntity bundleRequestEntity) {
        Optional<CiBundle> optCiBundle = ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundleRequestEntity.getIdBundle(), bundleRequestEntity.getCiFiscalCode());
        if (optCiBundle.isEmpty()) {
            ciBundleRepository.save(mapCiBundle(bundleRequestEntity));
        } else {
            CiBundle ciBundle = optCiBundle.get();
            ciBundle.setAttributes(bundleRequestEntity.getCiBundleAttributes());
            ciBundleRepository.save(ciBundle);
        }
    }

    /**
     * @param idPsp           PSP identifier
     * @param idBundleRequest Bundle Request identifier
     * @return the entity if exist
     * @throws AppException if not found
     */
    private BundleRequestEntity getBundleRequest(String idPsp, String idBundleRequest) {
        return bundleRequestRepository.findByIdAndIdPsp(idBundleRequest, idPsp)
                .orElseThrow(() -> new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, idBundleRequest));
    }

    private CiBundle mapCiBundle(BundleRequestEntity entity) {
        var startDate = entity.getValidityDateFrom() != null ?
                entity.getValidityDateFrom()
                : LocalDate.now().plusDays(1);
        var endDate = entity.getValidityDateTo();
        // check date: startDate must be >= now()+1day and endDate (if set) must be >= startDate+1day
        if (startDate.isBefore(LocalDate.now().plusDays(1))) {
            throw new AppException(AppError.BUNDLE_REQUEST_BAD_REQUEST, entity.getValidityDateFrom());
        }
        if (entity.getValidityDateTo() != null && endDate.isBefore(startDate.plusDays(1))) {
            throw new AppException(AppError.BUNDLE_REQUEST_BAD_REQUEST, entity.getValidityDateTo());
        }
        return CiBundle.builder()
                .ciFiscalCode(entity.getCiFiscalCode())
                .idBundle(entity.getIdBundle())
                .type(getBundle(entity.getIdBundle()).getType())
                .attributes(entity.getCiBundleAttributes())
                .validityDateFrom(startDate)
                .validityDateTo(endDate)
                .build();
    }

    /**
     * Archives a bundle request.
     * Create a new {@link ArchivedBundleRequest} entity and deletes the {@link BundleRequestEntity} entity.
     *
     * @param bundleRequestEntity an entity of {@link BundleRequestEntity}
     * @param accepted            true = request is accepted, false = not accepted, null = deleted
     */
    private void archiveBundleRequest(BundleRequestEntity bundleRequestEntity, Boolean accepted) {
        var requestToArchive = bundleRequestEntity;
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
        bundleRequestRepository.delete(bundleRequestEntity);
    }

    /**
     * Retrieve bundle
     *
     * @param idBundle bundle identifier
     * @return bundle bundle or not found exception
     */
    private Bundle getBundle(String idBundle) {
        Optional<Bundle> optBundle = bundleRepository.findById(idBundle);
        if (optBundle.isEmpty()) {
            throw new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        }
        return optBundle.get();
    }
}
