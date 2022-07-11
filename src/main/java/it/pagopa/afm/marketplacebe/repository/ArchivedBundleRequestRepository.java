package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.ArchivedBundleRequest;
import org.springframework.stereotype.Repository;

@Repository
public interface ArchivedBundleRequestRepository extends CosmosRepository<ArchivedBundleRequest, String> {

}
