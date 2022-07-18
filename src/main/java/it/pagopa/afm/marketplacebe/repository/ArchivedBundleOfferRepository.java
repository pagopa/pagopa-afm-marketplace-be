package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.ArchivedBundleOffer;
import org.springframework.stereotype.Repository;

@Repository
public interface ArchivedBundleOfferRepository extends CosmosRepository<ArchivedBundleOffer, String> {

}
