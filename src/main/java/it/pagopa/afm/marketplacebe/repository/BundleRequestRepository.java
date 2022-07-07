package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface BundleRequestRepository extends CosmosRepository<BundleRequest, String> {

    Page<BundleRequest> findByIdPsp(String idPsp, Pageable pageable);

    Page<BundleRequest> findByIdPspAndCiFiscalCode(String idPsp, String fiscalCode, Pageable pageable);

    List<BundleRequest> findByIdPsp(String idPsp);

    List<BundleRequest> findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(String idBundle, String idPsp);

    Optional<BundleRequest> findByIdAndIdPsp(String id, String idPsp);

    List<BundleRequest> findByCiFiscalCode(String fiscalCode);

    List<BundleRequest> findByCiFiscalCodeAndIdPsp(String fiscalCode, String idPsp);

}
