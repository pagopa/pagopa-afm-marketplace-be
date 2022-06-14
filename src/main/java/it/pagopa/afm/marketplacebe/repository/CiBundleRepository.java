package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.ReactiveCosmosRepository;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import org.springframework.stereotype.Repository;


@Repository
public interface CiBundleRepository extends ReactiveCosmosRepository<CiBundle, String> {

}
