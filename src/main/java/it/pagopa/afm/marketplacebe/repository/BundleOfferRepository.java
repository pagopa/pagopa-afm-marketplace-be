package it.pagopa.afm.marketplacebe.repository;

import com.azure.cosmos.models.PartitionKey;
import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface BundleOfferRepository extends CosmosRepository<BundleOffer, String> {
    Optional<BundleOffer> findById(String idBundleOffer, PartitionKey ciFiscalCode);

    List<BundleOffer> findByIdPsp(String idPsp);

    List<BundleOffer> findByIdPsp(String idPsp, PartitionKey ciFiscalCode);

    Page<BundleOffer> findByIdPsp(String idPsp, Pageable pageable);

    List<BundleOffer> findByIdPspAndIdBundleAndAcceptedDateIsNullAndRejectionDateIsNull(String idPsp, String idBundle);

    List<BundleOffer> findByCiFiscalCode(String ciFiscalCode);

}
