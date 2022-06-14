package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.ReactiveCosmosRepository;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface BundleOfferRepository extends ReactiveCosmosRepository<BundleOffer, String> {

    List<BundleOffer> findByIdPsp(String idPsp);
}
