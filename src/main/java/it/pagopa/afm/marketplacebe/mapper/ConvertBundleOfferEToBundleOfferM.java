package it.pagopa.afm.marketplacebe.mapper;

import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.Valid;

public class ConvertBundleOfferEToBundleOfferM implements Converter<BundleOffer, it.pagopa.afm.marketplacebe.model.offer.BundleOffer> {

    @Override
    public it.pagopa.afm.marketplacebe.model.offer.BundleOffer convert(MappingContext<BundleOffer, it.pagopa.afm.marketplacebe.model.offer.BundleOffer> context) {
        @Valid BundleOffer source = context.getSource();
        return it.pagopa.afm.marketplacebe.model.offer.BundleOffer
                .builder()
                .idBundleOffer(source.getId())
                .idBundle(source.getIdBundle())
                .ciFiscalCode(source.getCiFiscalCode())
                .acceptedDate(source.getAcceptedDate())
                .rejectionDate(source.getRejectionDate())
                .insertedDate(source.getInsertedDate())
                .build();
    }
}
