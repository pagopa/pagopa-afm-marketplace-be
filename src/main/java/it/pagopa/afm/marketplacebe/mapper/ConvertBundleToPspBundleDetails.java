package it.pagopa.afm.marketplacebe.mapper;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.model.bundle.PspBundleDetails;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.Valid;


public class ConvertBundleToPspBundleDetails implements Converter<Bundle, PspBundleDetails> {

    @Override
    public PspBundleDetails convert(MappingContext<Bundle, PspBundleDetails> context) {
        @Valid Bundle source = context.getSource();
        return PspBundleDetails.builder()
                .id(source.getId())
                .name(source.getName())
                .description(source.getDescription())
                .paymentAmount(source.getPaymentAmount())
                .minPaymentAmount(source.getMinPaymentAmount())
                .maxPaymentAmount(source.getMaxPaymentAmount())
                .paymentType(source.getPaymentType())
                .touchpoint(source.getTouchpoint())
                .type(source.getType().getValue())
                .transferCategoryList(source.getTransferCategoryList())
                .validityDateFrom(source.getValidityDateFrom())
                .validityDateTo(source.getValidityDateTo())
                .insertedDate(source.getInsertedDate())
                .lastUpdatedDate(source.getLastUpdatedDate())
                .idBrokerPsp(source.getIdBrokerPsp())
                .digitalStamp(source.getDigitalStamp())
                .digitalStampRestriction(source.getDigitalStampRestriction())
                .idChannel(source.getIdChannel())
                .pspBusinessName(source.getPspBusinessName())
                .urlPolicyPsp(source.getUrlPolicyPsp())
                .idPsp(source.getIdPsp())
                .cart(source.getCart())
                .abi(source.getAbi())
                .onUs(source.getOnUs())
                .build();
    }
}
