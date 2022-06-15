package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;

import java.util.List;

@Repository
public interface BundleOfferRepository extends CosmosRepository<BundleOffer, String> {

    List<BundleOffer> findByIdPsp(String idPsp);

    Page<BundleOffer> findByIdPsp(String idPsp, Pageable pageable);


}
