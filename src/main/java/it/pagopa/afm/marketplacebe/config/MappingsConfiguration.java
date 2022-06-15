package it.pagopa.afm.marketplacebe.config;

import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.mapper.ConvertBundleOfferEToBundleOfferM;
import it.pagopa.afm.marketplacebe.mapper.ConvertBundleRequestEToCiBundleRequestM;
import it.pagopa.afm.marketplacebe.model.request.CiBundleRequest;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MappingsConfiguration {

    @Bean
    public ModelMapper modelMapper() {

        ModelMapper mapper = new ModelMapper();
        mapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STANDARD);

        ConvertBundleOfferEToBundleOfferM convertBundleOfferEToBundleOfferM = new ConvertBundleOfferEToBundleOfferM();
        ConvertBundleRequestEToCiBundleRequestM convertBundleRequestEToCiBundleRequestM = new ConvertBundleRequestEToCiBundleRequestM();

        mapper.createTypeMap(BundleOffer.class, it.pagopa.afm.marketplacebe.model.offer.BundleOffer.class).setConverter(convertBundleOfferEToBundleOfferM);
        mapper.createTypeMap(BundleRequest.class, CiBundleRequest.class).setConverter(convertBundleRequestEToCiBundleRequestM);

        return mapper;
    }
}
