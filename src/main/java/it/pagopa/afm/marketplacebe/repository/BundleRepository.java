package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import org.springframework.stereotype.Repository;


@Repository
public interface BundleRepository extends CosmosRepository<Bundle, String> {}
