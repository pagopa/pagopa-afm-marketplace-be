package it.pagopa.afm.marketplacebe.config;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.mapper.ConvertBundleToPspBundleDetails;
import it.pagopa.afm.marketplacebe.mapper.ConvertCIBundleTCIBundleDetails;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleDetails;
import it.pagopa.afm.marketplacebe.model.bundle.PspBundleDetails;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MappingsConfiguration {

    @Bean
    public ModelMapper modelMapper() {

        ModelMapper mapper = new ModelMapper();
        mapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);

        mapper.createTypeMap(Bundle.class, PspBundleDetails.class).setConverter(new ConvertBundleToPspBundleDetails());
        mapper.createTypeMap(CiBundle.class, CiBundleDetails.class).setConverter(new ConvertCIBundleTCIBundleDetails());

        return mapper;
    }
}
