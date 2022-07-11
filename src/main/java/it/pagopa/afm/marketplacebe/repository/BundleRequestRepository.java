package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface BundleRequestRepository extends CosmosRepository<BundleRequestEntity, String> {

    Page<BundleRequestEntity> findByIdPsp(String idPsp, Pageable pageable);

    Page<BundleRequestEntity> findByIdPspAndCiFiscalCode(String idPsp, String fiscalCode, Pageable pageable);

    List<BundleRequestEntity> findByIdPsp(String idPsp);

    List<BundleRequestEntity> findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(String idBundle, String idPsp);

    Optional<BundleRequestEntity> findByIdAndIdPsp(String id, String idPsp);

    List<BundleRequestEntity> findByCiFiscalCode(String fiscalCode);

    List<BundleRequestEntity> findByCiFiscalCodeAndIdPsp(String fiscalCode, String idPsp);

}
