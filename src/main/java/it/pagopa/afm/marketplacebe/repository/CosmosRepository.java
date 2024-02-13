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

import java.util.List;
import java.util.StringJoiner;

@Repository
public class CosmosRepository {

    @Autowired
    CosmosTemplate cosmosTemplate;

    public List<Bundle> getBundlesByNameAndType(String idPsp, String name,
                                                List<BundleType> types, int pageNumber, int pageSize) {

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

        if(!result.isEmpty()) {
            return (int) Math.ceil((double) result.get(0) / pageSize);
        } else {
            return 0;
        }
    }

    private static void buildWhereConditions(String idPsp, String name, List<BundleType> types, StringBuilder builder) {
        // adds the idPsp clause if present
        if(StringUtils.isNotEmpty(idPsp)) {
            builder.append("AND b.idPsp = '" + idPsp + "' ");
        } else {
            // NOT idPsp search --> adds check on validityDateTo to return only valid bundles
            builder.append("AND (IS_NULL(b.validityDateTo) "
                    + "OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10)) ");
        }

        // adds the name clause
        if(StringUtils.isNotEmpty(name)) {
            builder.append("AND b.name like '%" + name + "%' ");
        }

        // adds the bundle types clause
        if(CollectionUtils.isNotEmpty(types)) {
            builder.append("AND b.type IN ( ");
            StringJoiner joiner = new StringJoiner(",");
            types.forEach(item -> joiner.add("'" + item.toString() + "'"));
            builder.append(joiner + " ) ");
        }
    }

}
