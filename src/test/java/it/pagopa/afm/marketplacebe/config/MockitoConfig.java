package it.pagopa.afm.marketplacebe.config;

import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import com.azure.spring.data.cosmos.core.CosmosTemplate;

import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.PaymentTypeRepository;
import it.pagopa.afm.marketplacebe.repository.TouchpointRepository;
import it.pagopa.afm.marketplacebe.repository.ValidBundleRepository;

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

    @Bean
    @Primary
    public ArchivedBundleRepository archivedBundleRepository() {
        return Mockito.mock(ArchivedBundleRepository.class);
    }

    @Bean
    @Primary
    public ArchivedCiBundleRepository archivedCiBundleRepository() {
        return Mockito.mock(ArchivedCiBundleRepository.class);
    }

    @Bean
    @Primary
    public TouchpointRepository touchpointRepository() {
        return Mockito.mock(TouchpointRepository.class);
    }

    @Bean
    @Primary
    public ValidBundleRepository validBundleRepository() {
        return Mockito.mock(ValidBundleRepository.class);
    }

    @Bean
    @Primary
    public PaymentTypeRepository paymentTypeRepository(){
        return Mockito.mock(PaymentTypeRepository.class);
    }
    
    @Bean
    @Primary
    public CosmosTemplate cosmosTemplate() {
      return Mockito.mock(CosmosTemplate.class);
    }

}
