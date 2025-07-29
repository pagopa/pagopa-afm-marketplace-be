package it.pagopa.afm.marketplacebe.service;


import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.repository.PaymentMethodRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
public class PaymentMethodsService {

    private final PaymentMethodRepository paymentMethodRepository;


    public List<PaymentMethod> getPaymentMethods() {
        return paymentMethodRepository.findAll();
    }

    public PaymentMethod getPaymentMethod(String paymentMethodId) {
        List<PaymentMethod> result = paymentMethodRepository.findByPaymentMethodId(paymentMethodId);
        if (result.isEmpty()) {
            throw new AppException(AppError.PAYMENT_METHOD_NOT_FOUND, paymentMethodId);
        }
        if (result.size() > 1) {
            throw new AppException(AppError.PAYMENT_METHOD_MULTIPLE_FOUND, paymentMethodId);
        }
        return result.get(0);
    }

    public PaymentMethod createPaymentMethod(PaymentMethod paymentMethod) {
        return paymentMethodRepository.save(paymentMethod);
    }

    public PaymentMethod updatePaymentMethod(String paymentMethodId, PaymentMethod paymentMethod) {
        PaymentMethod existing = getPaymentMethod(paymentMethodId);
        existing.setName(paymentMethod.getName());
        existing.setDescription(paymentMethod.getDescription());
        existing.setUserTouchpoint(paymentMethod.getUserTouchpoint());
        existing.setUserDevice(paymentMethod.getUserDevice());
        existing.setStatus(paymentMethod.getStatus());
        existing.setTypes(paymentMethod.getTypes());
        existing.setValidityDateFrom(paymentMethod.getValidityDateFrom());
        existing.setTarget(paymentMethod.getTarget());
        existing.setRangeAmount(paymentMethod.getRangeAmount());
        existing.setMetadata(paymentMethod.getMetadata());
        existing.setPaymentMethodAsset(paymentMethod.getPaymentMethodAsset());
        existing.setMethodManagement(paymentMethod.getMethodManagement());
        existing.setPaymentMethodsBrandAssets(paymentMethod.getPaymentMethodsBrandAssets());
        return paymentMethodRepository.save(existing);
    }

    public void deletePaymentMethod(String paymentMethodId) {
        PaymentMethod existing = getPaymentMethod(paymentMethodId);
        paymentMethodRepository.delete(existing);
    }
}
