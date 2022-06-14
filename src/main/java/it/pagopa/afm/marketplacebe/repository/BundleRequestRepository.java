package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.ReactiveCosmosRepository;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Repository
public interface BundleRequestRepository extends ReactiveCosmosRepository<BundleRequest, Long> {

    Flux<BundleRequest> findByIdPsp(String idPsp);

    Mono<BundleRequest> findByIdAndIdPsp(String id, String idPsp);
}
