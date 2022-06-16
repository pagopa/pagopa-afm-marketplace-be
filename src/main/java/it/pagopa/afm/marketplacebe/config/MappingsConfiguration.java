package it.pagopa.afm.marketplacebe.config;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.mapper.ConvertBundleEToBundleM;
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

        ConvertBundleEToBundleM convertBundleOfferEToBundleOfferM = new ConvertBundleEToBundleM();

        mapper.createTypeMap(Bundle.class, it.pagopa.afm.marketplacebe.model.bundle.Bundle.class).setConverter(convertBundleOfferEToBundleOfferM);

        return mapper;
    }
}