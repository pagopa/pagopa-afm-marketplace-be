package it.pagopa.afm.marketplacebe;

import static org.junit.jupiter.api.Assertions.assertTrue;

import com.azure.spring.data.cosmos.core.CosmosTemplate;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

@SpringBootTest
class MarketplaceBeApplicationTest {

  @MockBean private CosmosTemplate cosmosTemplate;

  @Test
  void contextLoads() {
    // check only if the context is loaded
    assertTrue(true);
  }
}
