package it.pagopa.afm.marketplacebe.config;

import it.pagopa.afm.marketplacebe.repository.*;
import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Configuration
public class MockitoConfig {

    @Bean
    @Primary
    public BundleOfferRepository bundleOfferRepository() {
        return Mockito.mock(BundleOfferRepository.class);
    }

    @Bean
    @Primary
    public BundleRepository bundleRepository() {
        return Mockito.mock(BundleRepository.class);
    }

    @Bean
    @Primary
    public BundleRequestRepository bundleRequestRepository() {
        return Mockito.mock(BundleRequestRepository.class);
    }

    @Bean
    @Primary
    public CiBundleRepository ciBundleRepository() {
        return Mockito.mock(CiBundleRepository.class);
    }


    @Bean
    @Primary
    public ArchivedBundleRequestRepository archivedBundleRequestRepository() {
        return Mockito.mock(ArchivedBundleRequestRepository.class);
    }

    @Bean
    @Primary
    public ArchivedBundleOfferRepository archivedBundleOfferRepository() {
        return Mockito.mock(ArchivedBundleOfferRepository.class);
    }

}
