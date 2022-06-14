package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.ReactiveCosmosRepository;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;

@Repository
public interface BundleRequestRepository extends ReactiveCosmosRepository<BundleRequest, Long> {

    Flux<BundleRequest> findByIdPsp(String idPsp);
}
