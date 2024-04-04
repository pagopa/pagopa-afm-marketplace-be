package it.pagopa.afm.marketplacebe.repository;

import com.azure.cosmos.models.PartitionKey;
import com.azure.spring.data.cosmos.repository.CosmosRepository;
import com.azure.spring.data.cosmos.repository.Query;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;


@Repository
public interface BundleRepository extends CosmosRepository<Bundle, String> {

    Optional<Bundle> findById(String idBundle);

    Optional<Bundle> findById(String idBundle, PartitionKey idPsp);

    List<Bundle> findByIdPsp(String idPsp, PartitionKey partitionKey);
    List<Bundle> findByIdPsp(String idPsp);

    @Query(value = "SELECT * " +
            "FROM bundles b " +
            "WHERE (IS_NULL(b.validityDateTo) OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], " +
            "b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10))")
    List<Bundle> getValidBundle();

    List<Bundle> findByIdPspAndTypeAndPaymentTypeAndTouchpoint(String idPsp, BundleType type, String paymentType, String touchpoint);
    
    List<Bundle> findByIdPspAndTypeAndTouchpointAndPaymentTypeIsNull (String idPsp, BundleType type, String touchpoint);

    List<Bundle> findByPaymentType(String paymentType);

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
    
    
    @Query(value = "SELECT * FROM bundles b"
    		+ " WHERE ("
    		+ "    SUBSTRING(DateTimeFromParts(b.validityDateFrom[0], b.validityDateFrom[1], b.validityDateFrom[2], 0, 0, 0, 0), 0, 10)"
    		+ "        <= "
    		+ "    SUBSTRING(DateTimeFromParts(@currentDate[0], @currentDate[1], @currentDate[2], 0, 0, 0, 0), 0, 10)"
    		+ " ) "
    		+ " AND ("
    		+ "    IS_NULL(b.validityDateTo)"
    		+ "    OR "
    		+ "    SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], b.validityDateTo[2], 0, 0, 0, 0), 0, 10)"
    		+ "        > "
    		+ "    SUBSTRING(DateTimeFromParts(@currentDate[0], @currentDate[1], @currentDate[2], 0, 0, 0, 0), 0, 10)"
    		+ " )")
    List<Bundle> findByCurrentDateBetweenValidityDateFromAndTo(@Param("currentDate") LocalDate currentDate);
    

}
