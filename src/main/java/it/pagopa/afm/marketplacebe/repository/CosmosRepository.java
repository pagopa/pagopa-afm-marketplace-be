package it.pagopa.afm.marketplacebe.repository;

import com.azure.cosmos.models.SqlQuerySpec;
import com.azure.spring.data.cosmos.core.CosmosTemplate;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.StringJoiner;
import java.util.stream.Collectors;

@Repository
public class CosmosRepository {

    private static final String BASE_BUNDLES_QUERY = "SELECT * FROM bundles b WHERE 1=1 ";
    private static final String AND_BUNDLE_NAME_LIKE = "AND b.name like '%";

    private final CosmosTemplate cosmosTemplate;

    @Autowired
    public CosmosRepository(CosmosTemplate cosmosTemplate) {
        this.cosmosTemplate = cosmosTemplate;
    }

    public List<Bundle> getBundlesByNameAndType(
            String idPsp,
            String name,
            List<BundleType> types,
            Sort.Direction maxPaymentAmountOrder, Long paymentAmountMinRange, Long paymentAmountMaxRange,
            int pageNumber,
            int pageSize
    ) {
        StringBuilder builder = new StringBuilder();
        builder.append(BASE_BUNDLES_QUERY);

        buildWhereConditions(idPsp, name, types, maxPaymentAmountOrder,  paymentAmountMinRange,  paymentAmountMaxRange, builder);

        builder.append("ORDER BY b.id OFFSET " + pageNumber * pageSize + " LIMIT " + pageSize);

        return IterableUtils
                .toList(cosmosTemplate.runQuery(new SqlQuerySpec(builder.toString()), Bundle.class, Bundle.class));
    }

    public Integer getTotalPages(String idPsp, String name, List<BundleType> types,Sort.Direction maxPaymentAmountOrder, Long paymentAmountMinRange, Long paymentAmountMaxRange, int pageSize) {
        StringBuilder builder = new StringBuilder();
        builder.append("SELECT VALUE COUNT(b.id) FROM bundles b WHERE 1=1 ");

        buildWhereConditions(idPsp, name, types, maxPaymentAmountOrder,  paymentAmountMinRange,  paymentAmountMaxRange, builder);

        List<Integer> result = IterableUtils
                .toList(cosmosTemplate.runQuery(new SqlQuerySpec(builder.toString()), Bundle.class, Integer.class));

        if (!result.isEmpty()) {
            return (int) Math.ceil((double) result.get(0) / pageSize);
        } else {
            return 0;
        }
    }

    private static void buildWhereConditions(String idPsp, String name, List<BundleType> types,Sort.Direction maxPaymentAmountOrder, Long paymentAmountMinRange, Long paymentAmountMaxRange, StringBuilder builder) {
        // adds the idPsp clause if present
        if (StringUtils.isNotEmpty(idPsp)) {
            builder.append("AND b.idPsp = '").append(idPsp).append("' ");
        } else {
            // NOT idPsp search --> adds check on validityDateTo to return only valid bundles
            builder.append("AND (IS_NULL(b.validityDateTo) "
                    + "OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10)) ");
        }

        // adds the name clause
        if (StringUtils.isNotEmpty(name)) {
            builder.append(AND_BUNDLE_NAME_LIKE).append(name).append("%' ");
        }

        // adds the bundle types clause
        if (CollectionUtils.isNotEmpty(types)) {
            builder.append("AND b.type IN ( ");
            StringJoiner joiner = new StringJoiner(",");
            types.forEach(item -> joiner.add("'" + item.toString() + "'"));
            builder.append(joiner).append(" ) ");
        }

        if (paymentAmountMinRange != null) {
            builder.append("AND b.paymentAmount > ").append(paymentAmountMinRange).append(" ");
        }

        if (paymentAmountMaxRange != null) {
            builder.append("AND b.paymentAmount < ").append(paymentAmountMaxRange).append(" ");
        }

        if (maxPaymentAmountOrder != null) {
            builder.append("AND ORDER BY b.maxPaymentAmount ").append(maxPaymentAmountOrder).append(" ");
        }
    }

    /**
     * Build and execute the query to get all bundles filtered by name, type and validity date on bundles table.
     * validFrom param is used for exclude all bundle that are not active before the specified date.
     *
     * @param name      bundle's name
     * @param types     list of bundle's types
     * @param validFrom validity date of bundles, used to retrieve all bundles valid from the specified date
     * @param expireAt   validity date of bundles, used to retrieve all bundles that expire at the specified date
     * @param offset    number of elements to be skipped
     * @param pageSize  page size
     * @return the requested page of bundles
     */
    public List<Bundle> getBundlesByNameAndTypeAndValidityDateFromAndExpireAt(
            String name,
            List<BundleType> types,
            LocalDate validFrom,
            LocalDate expireAt,
            int offset,
            int pageSize
    ) {
        StringBuilder builder = new StringBuilder();
        builder.append(BASE_BUNDLES_QUERY);

        buildWhereForGetBundlesByNameAndTypeAndValidityDateFromAndExpireAt(name, types, validFrom, expireAt, builder);

        builder.append("ORDER BY b.id OFFSET ").append(offset).append(" LIMIT ").append(pageSize);

        return IterableUtils
                .toList(cosmosTemplate.runQuery(new SqlQuerySpec(builder.toString()), Bundle.class, Bundle.class));
    }

    /**
     * Build and execute the query to get all bundles filtered by name, PSP business name and type on bundles table.
     *
     * @param name            bundle's name
     * @param bundleType      bundle's type
     * @param pspBusinessName PSP business name
     * @return the requested list of bundles
     */
    public List<Bundle> getBundlesByNameAndPSPBusinessName(
            String name,
            String pspBusinessName,
            String bundleType
    ) {
        StringBuilder builder = new StringBuilder();
        builder.append(BASE_BUNDLES_QUERY);

        buildWhereForGetBundlesByNameAndPSPBusinessName(name, pspBusinessName, bundleType, builder);

        return IterableUtils
                .toList(cosmosTemplate.runQuery(new SqlQuerySpec(builder.toString()), Bundle.class, Bundle.class));
    }

    /**
     * Build and execute the query to count all bundles filtered by name, type and validity date on bundles table.
     * validFrom param is used for exclude all bundle that are not active before the specified date.
     *
     * @param name      bundle's name
     * @param types     list of bundle's types
     * @param validFrom validity date of bundles, used to retrieve all bundles valid from the specified date
     * @param expireAt   validity date of bundles, used to retrieve all bundles that expire at the specified date
     * @return the number of element founds with the query
     */
    public Long getTotalItemsFindByNameAndTypeAndValidityDateFromAndExpireAt(
            String name,
            List<BundleType> types,
            LocalDate validFrom,
            LocalDate expireAt
    ) {
        StringBuilder builder = new StringBuilder();
        builder.append("SELECT VALUE COUNT(1) FROM bundles b WHERE 1=1 ");

        buildWhereForGetBundlesByNameAndTypeAndValidityDateFromAndExpireAt(name, types, validFrom, expireAt, builder);

        return cosmosTemplate.count(new SqlQuerySpec(builder.toString()), "bundles");
    }

    private void buildWhereForGetBundlesByNameAndTypeAndValidityDateFromAndExpireAt(
            String name,
            List<BundleType> types,
            LocalDate validFrom,
            LocalDate expireAt,
            StringBuilder builder
    ) {
        if (StringUtils.isNotEmpty(name)) {
            builder.append(AND_BUNDLE_NAME_LIKE).append(name).append("%' ");
        }

        if (CollectionUtils.isNotEmpty(types)) {
            builder.append("AND ARRAY_CONTAINS([")
                    .append(types.stream().map(Object::toString)
                            .collect(Collectors.joining("', '", "'", "'")))
                    .append("], b.type) ");
        }

        if (validFrom != null) {
            builder.append("AND SUBSTRING(DateTimeFromParts(b.validityDateFrom[0], b.validityDateFrom[1], b.validityDateFrom[2], 0, 0, 0, 0), 0, 10) ")
                    .append("<= SUBSTRING(DateTimeFromParts(")
                    .append(validFrom.getYear())
                    .append(",")
                    .append(validFrom.getMonthValue())
                    .append(",")
                    .append(validFrom.getDayOfMonth())
                    .append(", 0, 0, 0, 0), 0, 10) ");
        }

        if (expireAt != null) {
            builder.append("AND SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], b.validityDateTo[2], 0, 0, 0, 0), 0, 10) ")
                    .append("= SUBSTRING(DateTimeFromParts(")
                    .append(expireAt.getYear())
                    .append(",")
                    .append(expireAt.getMonthValue())
                    .append(",")
                    .append(expireAt.getDayOfMonth())
                    .append(", 0, 0, 0, 0), 0, 10) ");
        }
    }


    private void buildWhereForGetBundlesByNameAndPSPBusinessName(
            String name,
            String pspBusinessName,
            String bundleType,
            StringBuilder builder
    ) {
        if (StringUtils.isNotEmpty(name)) {
            builder.append(AND_BUNDLE_NAME_LIKE).append(name).append("%' ");
        }
        if (StringUtils.isNotEmpty(pspBusinessName)) {
            builder.append("AND b.pspBusinessName like '%").append(pspBusinessName).append("%' ");
        }

        if (StringUtils.isNotEmpty(bundleType)) {
            builder.append("AND b.type = '").append(bundleType).append("' ");
        }
    }
}
