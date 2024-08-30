package it.pagopa.afm.marketplacebe.repository;

import com.azure.cosmos.models.SqlQuerySpec;
import com.azure.spring.data.cosmos.core.CosmosTemplate;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import org.apache.tomcat.jni.Time;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Sort;

import java.time.LocalDate;
import java.util.List;

import static it.pagopa.afm.marketplacebe.TestUtil.getMockBundle;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@SpringBootTest(classes = CosmosRepository.class)
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

        List<Bundle> bundles = cosmosRepository.getBundlesByNameAndType(null, "mock name", List.of(BundleType.PRIVATE, BundleType.PUBLIC, BundleType.GLOBAL), Sort.Direction.ASC, 0L,0L, LocalDate.now(),LocalDate.now(),LocalDate.now(),LocalDate.now(),0, 50);

        assertFalse(bundles.isEmpty());
    }

    @Test
    void getBundlesByNameAndTypePspBundles() {
        var bundle = getMockBundle();
        Iterable<Object> bundleList = List.of(bundle);

        // Precondition
        when(cosmosTemplate.runQuery(any(SqlQuerySpec.class), any(), any())).thenReturn(bundleList);

        List<Bundle> bundles = cosmosRepository.getBundlesByNameAndType("idPsp", "mock name", List.of(BundleType.PRIVATE, BundleType.PUBLIC, BundleType.GLOBAL),  Sort.Direction.ASC, null, null,null,null,null,null, 0, 50);

        assertFalse(bundles.isEmpty());
    }

    @Test
    void getBundlesByNameAndTypeAndValidityDateFromAndExpireAt() {
        var bundle = getMockBundle();
        Iterable<Object> bundleList = List.of(bundle);

        // Precondition
        when(cosmosTemplate.runQuery(any(SqlQuerySpec.class), any(), any())).thenReturn(bundleList);

        List<Bundle> bundles = assertDoesNotThrow(() ->
                cosmosRepository.getBundlesByNameAndTypeAndValidityDateFromAndExpireAt(
                        "mock name",
                        List.of(BundleType.PRIVATE, BundleType.PUBLIC, BundleType.GLOBAL),
                        LocalDate.now(),
                        LocalDate.now(),
                        0,
                        50)
        );

        assertFalse(bundles.isEmpty());
    }

    @Test
    void getBundlesByNameAndPSPBusinessNameSuccess() {
        var bundle = getMockBundle();
        Iterable<Object> bundleList = List.of(bundle);

        // Precondition
        when(cosmosTemplate.runQuery(any(SqlQuerySpec.class), any(), any())).thenReturn(bundleList);

        List<Bundle> bundles = assertDoesNotThrow(() ->
                cosmosRepository.getBundlesByNameAndPSPBusinessName(
                        "mock name",
                        "pspBusinessName",
                        BundleType.PRIVATE.name())
        );

        assertFalse(bundles.isEmpty());
    }

    @Test
    void getBundlesByNameAndPSPBusinessNameSuccessWithNoFilter() {
        var bundle = getMockBundle();
        Iterable<Object> bundleList = List.of(bundle);

        // Precondition
        when(cosmosTemplate.runQuery(any(SqlQuerySpec.class), any(), any())).thenReturn(bundleList);

        List<Bundle> bundles = assertDoesNotThrow(() ->
                cosmosRepository.getBundlesByNameAndPSPBusinessName(
                        null,
                        null,
                        null)
        );

        assertFalse(bundles.isEmpty());
    }

    @Test
    void getTotalItemsFindByNameAndTypeAndValidityDateFromAndExpireAt() {
        // Precondition
        when(cosmosTemplate.count(any(SqlQuerySpec.class), anyString())).thenReturn(1L);

        Long result = assertDoesNotThrow(() ->
                cosmosRepository.getTotalItemsFindByNameAndTypeAndValidityDateFromAndExpireAt(
                        "mock name",
                        List.of(BundleType.PRIVATE, BundleType.PUBLIC, BundleType.GLOBAL),
                        LocalDate.now(),
                        LocalDate.now())
        );

        assertEquals(1L, result);
    }

}
