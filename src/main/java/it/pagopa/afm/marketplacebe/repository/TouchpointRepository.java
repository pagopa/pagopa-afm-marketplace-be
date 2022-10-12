package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;

public interface TouchpointRepository  extends CosmosRepository<Touchpoint, String> {
}
