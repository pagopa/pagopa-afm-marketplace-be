package it.pagopa.afm.marketplacebe.repository;

import com.azure.cosmos.models.SqlQuerySpec;
import com.azure.spring.data.cosmos.core.CosmosTemplate;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.StringJoiner;
import java.util.stream.Collectors;

@Repository
public class CosmosRepository {

    private final CosmosTemplate cosmosTemplate;

    @Autowired
    public CosmosRepository(CosmosTemplate cosmosTemplate) {
        this.cosmosTemplate = cosmosTemplate;
    }

    public List<Bundle> getBundlesByNameAndType(
            String idPsp,
            String name,
            List<BundleType> types,
            int pageNumber,
            int pageSize
    ) {
        StringBuilder builder = new StringBuilder();
        builder.append("SELECT * FROM bundles b WHERE 1=1 ");

        buildWhereConditions(idPsp, name, types, builder);

        builder.append("ORDER BY b.id OFFSET " + pageNumber * pageSize + " LIMIT " + pageSize);

        return IterableUtils
                .toList(cosmosTemplate.runQuery(new SqlQuerySpec(builder.toString()), Bundle.class, Bundle.class));
    }

    public Integer getTotalPages(String idPsp, String name, List<BundleType> types, int pageSize) {
        StringBuilder builder = new StringBuilder();
        builder.append("SELECT VALUE COUNT(b.id) FROM bundles b WHERE 1=1 ");

        buildWhereConditions(idPsp, name, types, builder);

        List<Integer> result = IterableUtils
                .toList(cosmosTemplate.runQuery(new SqlQuerySpec(builder.toString()), Bundle.class, Integer.class));

        if (!result.isEmpty()) {
            return (int) Math.ceil((double) result.get(0) / pageSize);
        } else {
            return 0;
        }
    }

    private static void buildWhereConditions(String idPsp, String name, List<BundleType> types, StringBuilder builder) {
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
            builder.append("AND b.name like '%").append(name).append("%' ");
        }

        // adds the bundle types clause
        if (CollectionUtils.isNotEmpty(types)) {
            builder.append("AND b.type IN ( ");
            StringJoiner joiner = new StringJoiner(",");
            types.forEach(item -> joiner.add("'" + item.toString() + "'"));
            builder.append(joiner).append(" ) ");
        }
    }

    /**
     * Build and execute the query to get all bundles filtered by name, type and validity date from on bundles table.
     * validFrom param is used for exclude all bundle that are not active before the specified date.
     *
     * @param name bundle's name
     * @param types list of bundle's types
     * @param validFrom validity date
     * @param offset number of elements to be skipped
     * @param pageSize page size
     * @return the requested page of bundles
     */
    public List<Bundle> getBundlesByNameAndTypeAndValidityDateFrom(
            String name,
            List<BundleType> types,
            LocalDate validFrom,
            int offset,
            int pageSize
    ) {
        StringBuilder builder = new StringBuilder();
        builder.append("SELECT * FROM bundles b WHERE 1=1 ");

        buildWhereForGetBundlesByNameAndTypeAndValidityDateFrom(name, types, validFrom, builder);

        builder.append("ORDER BY b.id OFFSET ").append(offset).append(" LIMIT ").append(pageSize);

        return IterableUtils
                .toList(cosmosTemplate.runQuery(new SqlQuerySpec(builder.toString()), Bundle.class, Bundle.class));
    }

    /**
     * Build and execute the query to count all bundles filtered by name, type and validity date from on bundles table.
     * validFrom param is used for exclude all bundle that are not active before the specified date.
     *
     * @param name bundle's name
     * @param types list of bundle's types
     * @param validFrom validity date
     * @return the number of element founds with the query
     */
    public Long getTotalItemsFindByNameAndTypeAndValidityDateFrom(
            String name,
            List<BundleType> types,
            LocalDate validFrom
    ) {
        StringBuilder builder = new StringBuilder();
        builder.append("SELECT VALUE COUNT(1) FROM bundles b WHERE 1=1 ");

        buildWhereForGetBundlesByNameAndTypeAndValidityDateFrom(name, types, validFrom, builder);

        return cosmosTemplate.count(new SqlQuerySpec(builder.toString()), "bundles");
    }

    private void buildWhereForGetBundlesByNameAndTypeAndValidityDateFrom(String name, List<BundleType> types, LocalDate validFrom, StringBuilder builder) {
        if (StringUtils.isNotEmpty(name)) {
            builder.append("AND b.name like '%").append(name).append("%' ");
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
    }

}
