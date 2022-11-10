package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;

import java.util.Optional;

public interface TouchpointRepository extends CosmosRepository<Touchpoint, String> {
    Optional<Touchpoint> findByName(String name);
}
