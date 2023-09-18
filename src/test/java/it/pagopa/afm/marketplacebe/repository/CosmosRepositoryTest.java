package it.pagopa.afm.marketplacebe.repository;

import static it.pagopa.afm.marketplacebe.TestUtil.getMockBundle;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import com.azure.cosmos.models.SqlQuerySpec;
import com.azure.spring.data.cosmos.core.CosmosTemplate;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;

@SpringBootTest
class CosmosRepositoryTest {
	
	@Autowired
	CosmosRepository cosmosRepository;
	
	@MockBean
	CosmosTemplate cosmosTemplate;
	
	@Test
	void getBundlesByNameAndTypeValidBundles() {
		
		var bundle = getMockBundle();
        Iterable<Object> bundleList = List.of(bundle);
        
        // Precondition
        when(cosmosTemplate.runQuery(any(SqlQuerySpec.class), any(), any())).thenReturn(bundleList);
		
        List<Bundle> bundles = cosmosRepository.getBundlesByNameAndType(null, "mock name", List.of(BundleType.PRIVATE, BundleType.PUBLIC, BundleType.GLOBAL), 0, 50);
        
        assertTrue(bundles.size() > 0);
	}
	
	@Test
	void getBundlesByNameAndTypePspBundles() {
		
		var bundle = getMockBundle();
        Iterable<Object> bundleList = List.of(bundle);
        
        // Precondition
        when(cosmosTemplate.runQuery(any(SqlQuerySpec.class), any(), any())).thenReturn(bundleList);
		
        List<Bundle> bundles = cosmosRepository.getBundlesByNameAndType("idPsp", "mock name", List.of(BundleType.PRIVATE, BundleType.PUBLIC, BundleType.GLOBAL), 0, 50);
        
        assertTrue(bundles.size() > 0);
	}

}
