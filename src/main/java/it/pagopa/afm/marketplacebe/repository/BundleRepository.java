package it.pagopa.afm.marketplacebe.repository;

import com.azure.cosmos.models.PartitionKey;
import com.azure.spring.data.cosmos.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
public interface BundleRepository extends CosmosRepository<Bundle, String> {
    Optional<Bundle> findById(String idBundle);
    Optional<Bundle> findById(String idBundle, PartitionKey idPsp);
    Page<Bundle> findById(String idBundle, Pageable pageable);

    List<Bundle> findByName(String name);
    Optional<Bundle> findByName(String name, PartitionKey idPsp);

    List<Bundle> findByIdPsp(String idPsp);

    List<Bundle> findByValidityDateToIsNullAndTypeIn(List<String> types);

    List<Bundle> findByTypeAndPaymentMethodAndTouchpoint(BundleType type, PaymentMethod paymentMethod, Touchpoint touchpoint);

    Page<Bundle> findByIdPsp(String idPsp, Pageable pageable);

}
