package it.pagopa.afm.marketplacebe.mapper;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.Valid;

public class ConvertBundleEToBundleM implements Converter<Bundle, it.pagopa.afm.marketplacebe.model.bundle.Bundle> {

    @Override
    public it.pagopa.afm.marketplacebe.model.bundle.Bundle convert(MappingContext<Bundle, it.pagopa.afm.marketplacebe.model.bundle.Bundle> context) {
        @Valid Bundle source = context.getSource();
        return it.pagopa.afm.marketplacebe.model.bundle.Bundle
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
