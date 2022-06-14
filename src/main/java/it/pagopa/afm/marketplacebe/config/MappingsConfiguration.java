package it.pagopa.afm.marketplacebe.config;

import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.mapper.ConvertBundleOfferEToBundleOfferM;
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

        ConvertBundleOfferEToBundleOfferM convertBundleOfferEToBundleOfferM = new ConvertBundleOfferEToBundleOfferM();

        mapper.createTypeMap(BundleOffer.class, it.pagopa.afm.marketplacebe.model.offer.BundleOffer.class).setConverter(convertBundleOfferEToBundleOfferM);

        return mapper;
    }
}
