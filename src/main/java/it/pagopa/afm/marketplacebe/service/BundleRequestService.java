package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
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

@Service
@Slf4j
public class BundleRequestService {

    @Autowired
    BundleRequestRepository bundleRequestRepository;

    @Autowired
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
