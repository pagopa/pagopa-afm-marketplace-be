package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.ArchivedBundle;
import org.springframework.stereotype.Repository;

@Repository
public interface ArchivedBundleRepository extends CosmosRepository<ArchivedBundle, String> {

}
