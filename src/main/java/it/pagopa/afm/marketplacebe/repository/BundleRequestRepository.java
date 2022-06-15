package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface BundleRequestRepository extends CosmosRepository<BundleRequest, String> {

    List<BundleRequest> findByCiFiscalCode(String fiscalCode);

    List<BundleRequest> findByCiFiscalCodeAndIdPsp(String fiscalCode, String idPsp);

}
