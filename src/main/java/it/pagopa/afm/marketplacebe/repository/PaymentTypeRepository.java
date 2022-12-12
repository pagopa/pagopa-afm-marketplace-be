package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.PaymentType;

import java.util.Optional;

public interface PaymentTypeRepository extends CosmosRepository<PaymentType, String> {
    Optional<PaymentType> findByName(String name);
}

