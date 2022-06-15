package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
public interface BundleRepository extends CosmosRepository<Bundle, String> {
    Optional<Bundle> findByIdBundle(String idBundle);

    List<Bundle> findByIdPsp(String idPsp);

    Page<Bundle> findByIdBundle(String idBundle, Pageable pageable);

    Page<Bundle> findByIdPsp(String idPsp, Pageable pageable);
}
