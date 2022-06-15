package it.pagopa.afm.marketplacebe.mapper;

import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.model.request.CiBundleRequest;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

public class ConvertBundleRequestEToCiBundleRequestM implements Converter<BundleRequest, CiBundleRequest> {

    @Override
    public CiBundleRequest convert(MappingContext<BundleRequest, CiBundleRequest> context) {
        @Valid BundleRequest source = context.getSource();

        List<CiBundleAttribute> attributes = new ArrayList<>();
        for(it.pagopa.afm.marketplacebe.entity.CiBundleAttribute attribute : source.getCiBundleAttributes()) {
            CiBundleAttribute attr = CiBundleAttribute.builder()
                    .maxPaymentAmount(attribute.getMaxPaymentAmount())
                    .transferCategory(attribute.getTransferCategory())
                    .transferCategoryRelation(attribute.getTransferCategoryRelation())
                    .build();
            attributes.add(attr);
        }

        return CiBundleRequest.builder()
                .idBundle(source.getIdBundle())
                .ciBundleAttributes(attributes)
                .build();

    }
}