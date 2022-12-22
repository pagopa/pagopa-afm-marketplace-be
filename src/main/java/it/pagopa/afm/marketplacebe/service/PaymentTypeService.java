package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.PaymentType;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentTypes;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.PaymentTypeRepository;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
@Slf4j
public class PaymentTypeService {

    @Autowired
    private BundleRepository bundleRepository;

    @Autowired
    private PaymentTypeRepository paymentTypeRepository;

    @Autowired
    private ModelMapper modelMapper;

    public PaymentTypes getPaymentTypes() {
        List<PaymentType> paymentTypes = new ArrayList<>();

        paymentTypeRepository
                .findAll()
                .forEach(p -> paymentTypes.add(modelMapper.map(p, PaymentType.class)));

        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(paymentTypes.size())
                .totalPages(1)
                .build();

        return PaymentTypes.builder()
                .paymentTypeList(paymentTypes)
                .pageInfo(pageInfo)
                .build();
    }

    public it.pagopa.afm.marketplacebe.model.paymenttype.PaymentType getPaymentType(String paymentTypeName) {
        PaymentType pt = paymentTypeRepository.findByName(paymentTypeName).orElseThrow(() -> new AppException(AppError.PAYMENT_TYPE_NOT_FOUND, paymentTypeName));
        List<Bundle> bundles = bundleRepository.findByPaymentType(pt.getName());

        return it.pagopa.afm.marketplacebe.model.paymenttype.PaymentType.builder()
                .name(paymentTypeName)
                .used(!bundles.isEmpty())
                .build();
    }

}
