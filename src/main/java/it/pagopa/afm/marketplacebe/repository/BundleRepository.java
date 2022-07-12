package it.pagopa.afm.marketplacebe.repository;

import com.azure.cosmos.models.PartitionKey;
import com.azure.cosmos.models.SqlParameter;
import com.azure.cosmos.models.SqlQuerySpec;
import com.azure.cosmos.util.CosmosPagedIterable;
import com.azure.spring.data.cosmos.repository.CosmosRepository;
import com.azure.spring.data.cosmos.repository.Query;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;


@Repository
public interface BundleRepository extends CosmosRepository<Bundle, String> {

    Optional<Bundle> findById(String idBundle);

    Optional<Bundle> findById(String idBundle, PartitionKey idPsp);

    Page<Bundle> findById(String idBundle, Pageable pageable);

    List<Bundle> findByName(String name);

    Optional<Bundle> findByName(String name, PartitionKey idPsp);

    Optional<Bundle> findByNameAndIdNot(String name, String id, PartitionKey idPsp);

    List<Bundle> findByIdPsp(String idPsp);

    @Query(value = "SELECT * " +
            "FROM bundles b " +
            "WHERE (IS_NULL(b.validityDateTo) OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], "+
            "b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10))")
    List<Bundle> getValidBundle();

    @Query(value = "SELECT * " +
            "FROM bundles b " +
            "WHERE (IS_NULL(b.validityDateTo) OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], "+
            "b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10)) AND b.type IN (@types)")
    List<Bundle> findByValidityDateToIsNullAndTypeIn(@Param("types") List<BundleType> types);

    List<Bundle> findByTypeAndPaymentMethodAndTouchpoint(BundleType type, PaymentMethod paymentMethod, Touchpoint touchpoint);

    Page<Bundle> findByIdPsp(String idPsp, Pageable pageable);

}
