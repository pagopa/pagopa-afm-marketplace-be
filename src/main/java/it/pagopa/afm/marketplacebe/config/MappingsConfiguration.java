package it.pagopa.afm.marketplacebe.config;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.mapper.ConvertBundleToPspBundleDetails;
import it.pagopa.afm.marketplacebe.model.bundle.PspBundleDetails;
import org.modelmapper.Converter;
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

        Converter<Bundle, PspBundleDetails> convertBundleToPspBundleDetails = new ConvertBundleToPspBundleDetails();
        mapper.createTypeMap(Bundle.class, PspBundleDetails.class).setConverter(convertBundleToPspBundleDetails);

        return mapper;
    }
}
