package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import org.springframework.stereotype.Repository;


@Repository
public interface CiBundleRepository extends CosmosRepository<CiBundle, String> {

}
