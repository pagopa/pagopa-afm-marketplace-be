package it.pagopa.afm.marketplacebe.repository;

import com.azure.cosmos.models.PartitionKey;
import com.azure.spring.data.cosmos.repository.CosmosRepository;
import com.azure.spring.data.cosmos.repository.Query;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface BundleOfferRepository extends CosmosRepository<BundleOffer, String> {
    Optional<BundleOffer> findById(String idBundleOffer, PartitionKey ciFiscalCode);

    List<BundleOffer> findByIdPsp(String idPsp);

    List<BundleOffer> findByIdPsp(String idPsp, PartitionKey ciFiscalCode);

    Page<BundleOffer> findByIdPsp(String idPsp, Pageable pageable);

    List<BundleOffer> findByIdPspAndIdBundleAndAcceptedDateIsNullAndRejectionDateIsNull(String idPsp, String idBundle);

    Optional<BundleOffer> findByIdBundleAndCiFiscalCodeAndAcceptedDateIsNullAndRejectionDateIsNull(String idBundle, String ciFiscalCode);

    List<BundleOffer> findByCiFiscalCode(String ciFiscalCode);

    @Query(value = "SELECT * " +
            "FROM bundleoffers b " +
            "WHERE (" +
            "SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], b.validityDateTo[2], 0, 0, 0, 0), 0, 10)" +
            " < " +
            "SUBSTRING(DateTimeFromParts(@currentDate[0], @currentDate[1], @currentDate[2], 0, 0, 0, 0), 0, 10)" +
            ")")
    List<BundleOffer> findByValidityDateToBefore(@Param("currentDate") LocalDate validityDateTo);

    @Query(value = "SELECT * " +
            "FROM bundleoffers b" +
            " WHERE " +
            " (IS_NULL(@idPsp) OR b.idPsp = @idPsp)" +
            " AND (IS_NULL(@ciFiscalCode) OR b.ciFiscalCode = @ciFiscalCode)" +
            " AND (IS_NULL(@idBundle) OR b.idBundle = @idBundle)" +
            " ORDER BY b.id OFFSET @offset LIMIT @pageSize")
    List<BundleOffer> findByIdPspAndFiscalCodeAndIdBundle(
            @Param("idPsp") String idPsp,
            @Param("ciFiscalCode") String ciFiscalCode,
            @Param("idBundle") String idBundle,
            @Param("offset") int offset,
            @Param("pageSize") int pageSize
    );

    @Query(value = "SELECT VALUE COUNT(1) " +
            "FROM bundleoffers b" +
            " WHERE " +
            " (IS_NULL(@idPsp) OR b.idPsp = @idPsp)" +
            " AND (IS_NULL(@ciFiscalCode) OR b.ciFiscalCode = @ciFiscalCode)" +
            " AND (IS_NULL(@idBundle) OR b.idBundle = @idBundle)")
    Integer getTotalItemsFindByIdPspAndFiscalCodeAndIdBundle(
            @Param("idPsp") String idPsp,
            @Param("ciFiscalCode") String ciFiscalCode,
            @Param("idBundle") String idBundle
    );

    @Query(value = "SELECT * " +
            "FROM bundleoffers b" +
            " WHERE " +
            " (IS_NULL(@idPsp) OR b.idPsp = @idPsp)" +
            " AND (IS_NULL(@ciFiscalCode) OR b.ciFiscalCode = @ciFiscalCode)" +
            " AND (IS_NULL(@idBundles) OR array_contains(@idBundles, b.idBundle))" +
            " ORDER BY b.id OFFSET @offset LIMIT @pageSize")
    List<BundleOffer> findByIdPspAndFiscalCodeAndIdBundles(
            @Param("idPsp") String idPsp,
            @Param("ciFiscalCode") String ciFiscalCode,
            @Param("idBundles") List<String> idBundles,
            @Param("offset") int offset,
            @Param("pageSize") int pageSize
    );

    @Query(value = "SELECT VALUE COUNT(1) " +
            "FROM bundleoffers b" +
            " WHERE " +
            " (IS_NULL(@idPsp) OR b.idPsp = @idPsp)" +
            " AND (IS_NULL(@ciFiscalCode) OR b.ciFiscalCode = @ciFiscalCode)" +
            " AND (IS_NULL(@idBundles) OR array_contains(@idBundles, b.idBundle))")
    Integer getTotalItemsFindByIdPspAndFiscalCodeAndIdBundles(
            @Param("idPsp") String idPsp,
            @Param("ciFiscalCode") String ciFiscalCode,
            @Param("idBundles") List<String> idBundles
    );
}
