package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.ArchivedCiBundle;
import org.springframework.stereotype.Repository;

@Repository
public interface ArchivedCiBundleRepository extends CosmosRepository<ArchivedCiBundle, String> {

}
