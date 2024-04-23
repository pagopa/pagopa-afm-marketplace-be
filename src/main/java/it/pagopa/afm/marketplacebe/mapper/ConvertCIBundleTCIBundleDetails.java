package it.pagopa.afm.marketplacebe.mapper;

import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleDetails;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

/**
 * Converter class that specify how to convert a {@link CiBundle} instance to a {@link CiBundleDetails} instance
 */
public class ConvertCIBundleTCIBundleDetails implements Converter<CiBundle, CiBundleDetails> {

    @Override
    public CiBundleDetails convert(MappingContext<CiBundle, CiBundleDetails> context) {
        @Valid CiBundle source = context.getSource();

        return CiBundleDetails.builder()
                .validityDateFrom(source.getValidityDateFrom())
                .validityDateTo(source.getValidityDateTo())
                .idCIBundle(source.getId())
                .ciTaxCode(source.getCiFiscalCode())
                .attributes(convertAttributes(source.getAttributes()))
                .build();
    }

    private List<CiBundleAttribute> convertAttributes(List<it.pagopa.afm.marketplacebe.entity.CiBundleAttribute> attributes) {
        if (attributes == null || attributes.isEmpty()) {
            return new ArrayList<>();
        }
        return attributes.stream()
                .map(attribute -> CiBundleAttribute.builder()
                        .maxPaymentAmount(attribute.getMaxPaymentAmount())
                        .transferCategory(attribute.getTransferCategory())
                        .insertedDate(attribute.getInsertedDate())
                        .transferCategoryRelation(attribute.getTransferCategoryRelation())
                        .build())
                .toList();
    }
}
