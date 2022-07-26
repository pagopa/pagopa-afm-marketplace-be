package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import com.azure.spring.data.cosmos.repository.Query;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;


@Repository
public interface CiBundleRepository extends CosmosRepository<CiBundle, String> {

    List<CiBundle> findByIdBundle(String idBundle);

    List<CiBundle> findByCiFiscalCode(String ciFiscalCode);

    Optional<CiBundle> findByIdBundleAndCiFiscalCode(String idBundle, String ciFiscalCode);

    Optional<CiBundle> findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(String idBundle, String ciFiscalCode);

    @Query(value = "SELECT * " +
            "FROM cibundles b " +
            "WHERE (" +
            "SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], b.validityDateTo[2], 0, 0, 0, 0), 0, 10)" +
            " < " +
            "SUBSTRING(DateTimeFromParts(@currentDate[0], @currentDate[1], @currentDate[2], 0, 0, 0, 0), 0, 10)" +
            ")")
    List<CiBundle> findByValidityDateToBefore(@Param("currentDate") LocalDate validityDateTo);
}
