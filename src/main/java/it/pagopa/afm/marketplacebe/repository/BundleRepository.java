package it.pagopa.afm.marketplacebe.repository;

import com.azure.cosmos.models.PartitionKey;
import com.azure.spring.data.cosmos.repository.CosmosRepository;
import com.azure.spring.data.cosmos.repository.Query;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.PaymentType;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;


@Repository
public interface BundleRepository extends CosmosRepository<Bundle, String> {

    Optional<Bundle> findById(String idBundle);

    Optional<Bundle> findById(String idBundle, PartitionKey idPsp);

    Optional<Bundle> findByNameAndIdPsp(String name, String idPsp, PartitionKey partitionKey);

    Optional<Bundle> findByNameAndIdNot(String name, String id, PartitionKey idPsp);

    List<Bundle> findByIdPsp(String idPsp, PartitionKey partitionKey);

    @Query(value = "SELECT * " +
            "FROM bundles b " +
            "WHERE (IS_NULL(b.validityDateTo) OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], " +
            "b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10))")
    List<Bundle> getValidBundle();

    @Query(value = "SELECT * " +
            "FROM bundles b " +
            "WHERE (IS_NULL(b.validityDateTo) OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], " +
            "b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10)) AND b.type IN (@type0)")
    List<Bundle> getValidBundleByType(@Param("type0") String type0);

    @Query(value = "SELECT * " +
            "FROM bundles b " +
            "WHERE (IS_NULL(b.validityDateTo) OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], " +
            "b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10)) AND b.type IN (@type0, @type1)")
    List<Bundle> getValidBundleByType(@Param("type0") String type0, @Param("type1") String type1);

    @Query(value = "SELECT * " +
            "FROM bundles b " +
            "WHERE (IS_NULL(b.validityDateTo) OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], " +
            "b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10)) AND b.type IN (@type0, @type1, @type2)")
    List<Bundle> getValidBundleByType(@Param("type0") String type0, @Param("type1") String type1, @Param("type2") String type2);

    List<Bundle> findByIdPspAndTypeAndPaymentMethodAndTouchpoint(String idPsp, BundleType type, PaymentType paymentType, String touchpoint);

    @Query(value = "SELECT * " +
            "FROM bundles b " +
            "WHERE (" +
            "SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], b.validityDateTo[2], 0, 0, 0, 0), 0, 10)" +
            " < " +
            "SUBSTRING(DateTimeFromParts(@currentDate[0], @currentDate[1], @currentDate[2], 0, 0, 0, 0), 0, 10)" +
            ")")
    List<Bundle> findByValidityDateToBefore(@Param("currentDate") LocalDate validityDateTo);


    @Query(value = "SELECT * " +
            "FROM bundles b " +
            "WHERE (IS_NULL(b.validityDateTo) OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], " +
            "b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10)) AND b.touchpoint like @touchpointName")
    List<Bundle> findByTouchpointAndValid(String touchpointName);

}
