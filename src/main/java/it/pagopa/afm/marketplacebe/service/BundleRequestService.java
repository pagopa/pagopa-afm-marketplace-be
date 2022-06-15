package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.request.CiBundleRequest;
import it.pagopa.afm.marketplacebe.model.request.CiRequests;
import it.pagopa.afm.marketplacebe.model.request.Requests;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
public class BundleRequestService {

    @Autowired
    BundleRequestRepository bundleRequestRepository;

    @Autowired
    CiBundleRepository ciBundleRepository;

    @Autowired
    ModelMapper modelMapper;

    /**
     * Get requests for psp
     * @param ciFiscalCode
     * @param size
     * @param cursor
     * @param idPsp optional
     * @return
     */
    public Mono<Requests> getRequestsByPSP(String idPsp, Integer size, String cursor, String ciFiscalCode) {
        // TODO: pageable
        // TODO: filter
        var requests = bundleRequestRepository.findByIdPsp(idPsp);

        return requests.collectList()
                .map(list -> Requests.builder()
                        .requestsList(list)
                        .build());
    }

    public void acceptRequest(String idPsp, String idBundleRequest) {
         bundleRequestRepository.findByIdAndIdPsp(idBundleRequest, idPsp)
                .switchIfEmpty(Mono.error(new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, idBundleRequest)))
                .flatMap(entity -> bundleRequestRepository.save(entity.toBuilder()
                        .acceptedDate(LocalDateTime.now())
                        .build()))
                .flatMap(entity ->
                        ciBundleRepository.save(buildEcBundle(entity)))
                .subscribe();
    }


    public Mono<BundleRequest> rejectRequest(String idPsp, String idBundleRequest) {
        return bundleRequestRepository.findByIdAndIdPsp(idBundleRequest, idPsp)
                .switchIfEmpty(Mono.error(new AppException(AppError.BUNDLE_REQUEST_NOT_FOUND, idBundleRequest)))
                .flatMap(entity -> bundleRequestRepository.save(entity.toBuilder()
                        .rejectionDate(LocalDateTime.now())
                        .build()));
    }

    /**
     * Get requests for creditor institution
     * @param ciFiscalCode
     * @param size
     * @param cursor
     * @param idPsp optional
     * @return
     */
    public Mono<CiRequests> getRequestsByCI(String ciFiscalCode, Integer size, String cursor, String idPsp) {
        // TODO: pageable
        // TODO: filter
        Flux<BundleRequest> requests = bundleRequestRepository.findByCiFiscalCode(ciFiscalCode);

        Mono<List<CiBundleRequest>> ciBundleRequestList = requests.collectList()
                .map(list -> list.stream().map(request -> modelMapper.map(request, CiBundleRequest.class)).collect(Collectors.toList()));

        return ciBundleRequestList.map(list ->
                CiRequests.builder()
                        .requestsList(list)
                        .build());
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
