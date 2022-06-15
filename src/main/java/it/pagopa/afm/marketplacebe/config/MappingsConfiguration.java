package it.pagopa.afm.marketplacebe.config;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.mapper.ConvertBundleEToBundleM;
import it.pagopa.afm.marketplacebe.mapper.ConvertBundleRequestEToCiBundleRequestM;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetails;
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

        ConvertBundleRequestEToCiBundleRequestM convertBundleRequestEToCiBundleRequestM = new ConvertBundleRequestEToCiBundleRequestM();
        ConvertBundleEToBundleM convertBundleOfferEToBundleOfferM = new ConvertBundleEToBundleM();

        mapper.createTypeMap(BundleRequest.class, CiBundleRequest.class).setConverter(convertBundleRequestEToCiBundleRequestM);
        mapper.createTypeMap(Bundle.class, BundleDetails.class).setConverter(convertBundleOfferEToBundleOfferM);

        return mapper;
    }
}
