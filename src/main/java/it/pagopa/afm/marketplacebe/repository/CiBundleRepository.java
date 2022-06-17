package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
public interface CiBundleRepository extends CosmosRepository<CiBundle, String> {

    Optional<CiBundle> findByIdBundleAndCiFiscalCode(String idBundle, String ciFiscalCode);
    List<CiBundle> findByIdBundle(String idBundle);

    List<CiBundle> findByCiFiscalCode(String ciFiscalCode);

}
