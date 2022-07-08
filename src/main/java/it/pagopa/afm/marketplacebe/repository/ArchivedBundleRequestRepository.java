package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.ArchivedBundleRequest;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface ArchivedBundleRequestRepository extends CosmosRepository<ArchivedBundleRequest, String> {

    Page<ArchivedBundleRequest> findByIdPsp(String idPsp, Pageable pageable);

    Page<ArchivedBundleRequest> findByIdPspAndCiFiscalCode(String idPsp, String fiscalCode, Pageable pageable);

    List<ArchivedBundleRequest> findByIdPsp(String idPsp);

    List<ArchivedBundleRequest> findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(String idBundle, String idPsp);

    Optional<ArchivedBundleRequest> findByIdAndIdPsp(String id, String idPsp);

    List<ArchivedBundleRequest> findByCiFiscalCode(String fiscalCode);

    List<ArchivedBundleRequest> findByCiFiscalCodeAndIdPsp(String fiscalCode, String idPsp);

}
