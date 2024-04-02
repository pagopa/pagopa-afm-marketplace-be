package it.pagopa.afm.marketplacebe.repository;

import com.azure.spring.data.cosmos.repository.CosmosRepository;
import com.azure.spring.data.cosmos.repository.Query;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface BundleRequestRepository extends CosmosRepository<BundleRequestEntity, String> {

    Page<BundleRequestEntity> findByIdPsp(String idPsp, Pageable pageable);

    Page<BundleRequestEntity> findByIdPspAndCiFiscalCode(String idPsp, String fiscalCode, Pageable pageable);

    List<BundleRequestEntity> findByIdPsp(String idPsp);

    List<BundleRequestEntity> findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(String idBundle, String idPsp);

    Optional<BundleRequestEntity> findByIdAndIdPsp(String id, String idPsp);

    List<BundleRequestEntity> findByCiFiscalCode(String fiscalCode);

    List<BundleRequestEntity> findByCiFiscalCodeAndIdPsp(String fiscalCode, String idPsp);
    @Query(value = "SELECT * " +
            "FROM bundlerequests b" +
            " WHERE " +
            "b.idPsp = @idPsp" +
            " AND (IS_NULL(@ciFiscalCode) OR b.ciFiscalCode = @ciFIscalCode)" +
            " AND (IS_NULL(@idBundle) OR b.idBundle = @idBundle)" +
            " ORDER BY b.id OFFSET @offset LIMIT @pageSize")
    List<BundleRequestEntity> findByIdPspAndFiscalCodeAndType(@Param("idPsp") String idPsp,  @Param("ciFiscalCode") String ciFiscalCode, @Param("idBundle") String idBundle,
                                                              @Param("offset") int offset, @Param("pageSize") int pageSize);

    @Query(value = "SELECT * " +
            "FROM bundlerequests b " +
            "WHERE (" +
            "SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], b.validityDateTo[2], 0, 0, 0, 0), 0, 10)" +
            " < " +
            "SUBSTRING(DateTimeFromParts(@currentDate[0], @currentDate[1], @currentDate[2], 0, 0, 0, 0), 0, 10)" +
            ")")
    List<BundleRequestEntity> findByValidityDateToBefore(@Param("currentDate") LocalDate validityDateTo);

}
