package it.pagopa.afm.marketplacebe.repository;

import java.util.List;
import java.util.StringJoiner;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.azure.cosmos.models.SqlQuerySpec;
import com.azure.spring.data.cosmos.core.CosmosTemplate;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;

@Repository
public class CosmosRepository {

	@Autowired
	CosmosTemplate cosmosTemplate;

	public List<Bundle> getBundlesByNameAndType(String idPsp, String name,
			List<BundleType> types, int offset, int limit) {

		StringBuilder builder = new StringBuilder();
		builder.append("SELECT * FROM bundles b WHERE 1=1 ");

		// adds the idPsp clause if present
		if (StringUtils.isNotEmpty(idPsp)) {
			builder.append("AND b.idPsp = '" + idPsp + "' ");
		} else {
			// NOT idPsp search --> adds check on validityDateTo to return only valid bundles
			builder.append("AND (IS_NULL(b.validityDateTo) "
					+ "OR SUBSTRING(DateTimeFromParts(b.validityDateTo[0], b.validityDateTo[1], b.validityDateTo[2], 0, 0, 0, 0), 0, 10) > SUBSTRING(GetCurrentDateTime(), 0, 10)) ");
		}

		// adds the name clause
		if (StringUtils.isNotEmpty(name)) {
			builder.append("AND b.name like '%" + name + "%' ");
		}

		// adds the bundle types clause
		if (CollectionUtils.isNotEmpty(types)) {
			builder.append("AND b.type IN ( ");
			StringJoiner joiner = new StringJoiner(",");
			types.forEach(item -> joiner.add("'" + item.toString() + "'"));
			builder.append(joiner.toString() + " ) ");
		}

		builder.append("ORDER BY b.id OFFSET " + offset + " LIMIT " + limit);

		return IterableUtils
				.toList(cosmosTemplate.runQuery(new SqlQuerySpec(builder.toString()), Bundle.class, Bundle.class));
	}

}
