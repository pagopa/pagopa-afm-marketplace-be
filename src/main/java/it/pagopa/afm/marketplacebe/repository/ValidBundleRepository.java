package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.ValidBundle;
import org.springframework.stereotype.Repository;


@Repository
public interface ValidBundleRepository extends CosmosRepository<ValidBundle, String> {

}
