package it.pagopa.afm.marketplacebe;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.retry.annotation.EnableRetry;

import static org.junit.jupiter.api.Assertions.assertTrue;

@SpringBootTest
class MarketplaceBeApplicationTest {


    @Test
    void contextLoads() {
        // check only if the context is loaded
        assertTrue(true);
    }

}
