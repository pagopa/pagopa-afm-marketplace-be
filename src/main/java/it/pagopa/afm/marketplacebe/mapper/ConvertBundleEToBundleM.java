package it.pagopa.afm.marketplacebe.mapper;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetails;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.Valid;

public class ConvertBundleEToBundleM implements Converter<Bundle, BundleDetails> {

    @Override
    public BundleDetails convert(MappingContext<Bundle, BundleDetails> context) {
        @Valid Bundle source = context.getSource();
        return BundleDetails
                .builder()
                .idBundle(source.getIdBundle())
                .idPsp(source.getIdPsp())
                .name(source.getName())
                .description(source.getDescription())
                .paymentAmount(source.getPaymentAmount())
                .minPaymentAmount(source.getMinPaymentAmount())
                .maxPaymentAmount(source.getMaxPaymentAmount())
                .paymentMethod(source.getPaymentMethod().name())
                .touchpoint(source.getTouchpoint().name())
                .type(source.getType().name())
                .transferCategoryList(source.getTransferCategoryList())
                .validityDateFrom(source.getValidityDateFrom())
                .validityDateTo(source.getValidityDateTo())
                .insertedDate(source.getInsertedDate())
                .lastUpdatedDate(source.getLastUpdatedDate())
                .build();
    }
}
