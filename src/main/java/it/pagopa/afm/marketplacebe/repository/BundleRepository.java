package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public interface BundleRepository extends CosmosRepository<Bundle, String> {
    List<Bundle> findByIdBundle(String idBundle);
    Page<Bundle> findByIdBundle(String idBundle, Pageable pageable);

    List<Bundle> findByIdPsp(String idPsp);
    Page<Bundle> findByIdPsp(String idPsp, Pageable pageable);
}
