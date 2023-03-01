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

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

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

    public List<PaymentType> uploadPaymentTypeByList(List<String> paymentTypeList) {

        Set<String> paymentTypes = new HashSet<>(paymentTypeList);
        paymentTypes.stream()
                .map(paymentType -> paymentTypeRepository.findByName(paymentType).orElse(null))
                .filter(paymentTypeEntity -> paymentTypeEntity != null && !bundleRepository.findByPaymentType(paymentTypeEntity.getName()).isEmpty())
                .findAny()
                .ifPresent(paymentType -> {
                    throw new AppException(AppError.PAYMENT_TYPE_NOT_DELETABLE, paymentType.getName());
                });

        List<PaymentType> paymentTypeEntityList = new LinkedList<>();
        int paymentTypeGeneratedId = 1;
        for (String paymentTypeName : paymentTypes) {
            paymentTypeEntityList.add(
                    PaymentType.builder()
                            .id(String.valueOf(paymentTypeGeneratedId))
                            .name(paymentTypeName)
                            .createdDate(LocalDateTime.now())
                            .build()
            );
            paymentTypeGeneratedId++;
        }

        paymentTypeRepository.deleteAll();
        paymentTypeRepository.saveAll(paymentTypeEntityList);
        return paymentTypeEntityList;
    }
}
