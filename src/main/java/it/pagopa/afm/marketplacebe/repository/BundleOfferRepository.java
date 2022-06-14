package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.ReactiveCosmosRepository;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;

import java.util.List;

@Repository
public interface BundleOfferRepository extends ReactiveCosmosRepository<BundleOffer, String> {

    Flux<BundleOffer> findByIdPsp(String idPsp);
}
