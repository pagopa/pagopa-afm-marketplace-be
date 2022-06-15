package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.model.request.*;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import org.modelmapper.ModelMapper;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.request.Requests;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Service

@Service
@Slf4j
public class BundleRequestService {

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

    public BundleRequestId createRequest(String ciFiscalCode, CiBundleSubscriptionRequest ciBundleSubscriptionRequest) {

        // TODO retrieve bundle by idBundle
        Bundle bundle = new Bundle();

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
                .idBundle(ciBundleSubscriptionRequest.getIdBundle())
                .ciFiscalCode(ciFiscalCode)
                .idPsp(bundle.getIpPsP())
                .ciBundleAttributes(attributes)
                .build();

        bundleRequestRepository.save(request);

        return BundleRequestId.builder().idBundleRequest(request.getId()).build();
    CiBundleRepository ciBundleRepository;

    public Mono<Requests> getRequests(String idPsp, Integer size, String cursor, String ciFiscalCode) {
        // TODO: pageable
        // TODO: filter
        var requests = bundleRequestRepository.findByIdPsp(idPsp);

        return requests.collectList()
                .map(list -> Requests.builder()
                        .requestsList(list)
                        .build());
    }

    public Mono<Void> acceptRequest(String idPsp, String idBundleRequest) {
        return bundleRequestRepository.findByIdAndIdPsp(idBundleRequest, idPsp)
                .switchIfEmpty(Mono.error(new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, idBundleRequest)))
                .flatMap(entity -> bundleRequestRepository.save(entity.toBuilder()
                        .acceptedDate(LocalDateTime.now())
                        .build()))
                .flatMap(entity ->
                        ciBundleRepository.save(buildEcBundle(entity)))
                .and(Mono.empty());
    }


    public Mono<Void> rejectRequest(String idPsp, String idBundleRequest) {
        return bundleRequestRepository.findByIdAndIdPsp(idBundleRequest, idPsp)
                .switchIfEmpty(Mono.error(new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, idBundleRequest)))
                .flatMap(entity -> bundleRequestRepository.save(entity.toBuilder()
                        .rejectionDate(LocalDateTime.now())
                        .build()))
                .and(Mono.empty());
    }

    private CiBundle buildEcBundle(BundleRequest entity) {
        return CiBundle.builder()
                .ciFiscalCode(entity.getCiFiscalCode())
                .bundle(Bundle.builder()
                        .id(entity.getIdBundle())
                        .build())
                .attributes(entity.getCiBundleAttributes())
                .build();
    }
}
