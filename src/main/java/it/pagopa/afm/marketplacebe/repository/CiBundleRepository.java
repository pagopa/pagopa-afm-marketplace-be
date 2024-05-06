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

    @Query(value = "SELECT * FROM cibundles c " +
            "WHERE c.ciFiscalCode = @ciFiscalCode " +
            "AND (IS_NULL(@type) OR c.type = @type)" +
            "AND (IS_NULL(@idBundles) OR array_contains(@idBundles, c.idBundle)) " +
            "ORDER BY c.id OFFSET @offset LIMIT @pageSize ")
    List<CiBundle> findByCiFiscalCodeAndTypeAndIdBundles(
            @Param("ciFiscalCode") String ciFiscalCode,
            @Param("type") String type,
            @Param("idBundles") List<String> idBundles,
            @Param("offset") int offset,
            @Param("pageSize") int pageSize
    );

    @Query(value = "SELECT * FROM cibundles c " +
            "WHERE c.ciFiscalCode = @ciFiscalCode " +
            "AND (IS_NULL(@type) OR c.type = @type)" +
            "AND (IS_NULL(@idBundles) OR array_contains(@idBundles, c.idBundle))")
    Integer getTotalItemsFindByCiFiscalCodeAndTypeAndIdBundles(
            @Param("ciFiscalCode") String ciFiscalCode,
            @Param("type") String type,
            @Param("idBundles") List<String> idBundles
    );

    @Query(value = "SELECT * " +
            "FROM cibundles c " +
            "WHERE c.idBundle = @idBundle " +
            "AND (IS_NULL(@ciFiscalCode) OR c.ciFiscalCode = @ciFiscalCode) " +
            "ORDER BY c.id OFFSET @offset LIMIT @pageSize")
    List<CiBundle> findByIdBundleAndCiFiscalCode(
            @Param("idBundle") String idBundle,
            @Param("ciFiscalCode") String ciFiscalCode,
            @Param("offset") int offset,
            @Param("pageSize") int pageSize
    );

    @Query(value = "SELECT VALUE COUNT(1) " +
            "FROM cibundles c " +
            "WHERE c.idBundle = @idBundle " +
            "AND (IS_NULL(@ciFiscalCode) OR c.ciFiscalCode = @ciFiscalCode)")
    Integer getTotalItemsFindByIdBundleAndCiFiscalCode(
            @Param("idBundle") String idBundle,
            @Param("ciFiscalCode") String ciFiscalCode
    );

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
