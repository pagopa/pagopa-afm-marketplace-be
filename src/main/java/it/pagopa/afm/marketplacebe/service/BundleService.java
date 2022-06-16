package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetails;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetailsAttributes;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.bundle.BundleResponse;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Transactional
public class BundleService {

    @Autowired
    private BundleRepository bundleRepository;

    @Autowired
    private CiBundleRepository ciBundleRepository;

    @Autowired
    private ModelMapper modelMapper;

    // TODO: add pagination
    // TODO: add filter
    public Bundles getBundlesByIdPsp(String idPsp, Integer pageNumber, Integer limit) {
        List<BundleDetails> bundleList = bundleRepository
                .findByIdPsp(idPsp)
                .stream()
                .map(bundle -> modelMapper.map(bundle, BundleDetails.class))
                .collect(Collectors.toList());

        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(bundleList.size())
                .totalPages(1)
                .build();

        return Bundles.builder()
                .bundleDetailsList(bundleList)
                .pageInfo(pageInfo)
                .build();
    }

    public BundleResponse createBundle(String idPsp, BundleRequest bundleRequest) {

        LocalDateTime validityDateFrom = bundleRequest.getValidityDateFrom() != null ? bundleRequest.getValidityDateFrom() : LocalDateTime.now();
        if (bundleRequest.getValidityDateTo() != null && bundleRequest.getValidityDateTo().isBefore(validityDateFrom)) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDateTo is null or before ValidityDateFrom");
        }

        LocalDateTime now = LocalDateTime.now();
        Bundle bundle = Bundle.builder()
                .idPsp(idPsp)
                .name(bundleRequest.getName())
                .description(bundleRequest.getDescription())
                .paymentAmount(bundleRequest.getPaymentAmount())
                .minPaymentAmount(bundleRequest.getMinPaymentAmount())
                .maxPaymentAmount(bundleRequest.getMaxPaymentAmount())
                .paymentMethod(PaymentMethod.valueOf(bundleRequest.getPaymentMethod()))
                .touchpoint(Touchpoint.valueOf(bundleRequest.getTouchpoint()))
                .type(BundleType.valueOf(bundleRequest.getType()))
                .transferCategoryList(bundleRequest.getTransferCategoryList())
                .validityDateFrom(validityDateFrom)
                .validityDateTo(bundleRequest.getValidityDateTo())
                .insertedDate(now)
                .lastUpdatedDate(now)
                .build();
        bundleRepository.save(bundle);
        return BundleResponse.builder()
                .idBundle(bundle.getId())
                .build();
    }

    public Bundle updateBundle(String idPsp, String idBundle, BundleRequest bundleRequest) {
        Bundle bundle = getBundle(idBundle, idPsp);

        bundle.setName(bundleRequest.getName());
        bundle.setDescription(bundleRequest.getDescription());
        bundle.setPaymentAmount(bundleRequest.getPaymentAmount());
        bundle.setMinPaymentAmount(bundleRequest.getMinPaymentAmount());
        bundle.setMaxPaymentAmount(bundleRequest.getMaxPaymentAmount());
        bundle.setPaymentAmount(bundleRequest.getPaymentAmount());
        bundle.setTouchpoint(Touchpoint.valueOf(bundleRequest.getTouchpoint()));
        bundle.setType(BundleType.valueOf(bundleRequest.getType()));
        bundle.setTransferCategoryList(bundleRequest.getTransferCategoryList());
        bundle.setValidityDateFrom(bundleRequest.getValidityDateFrom());
        bundle.setValidityDateTo(bundleRequest.getValidityDateTo());
        bundle.setLastUpdatedDate(LocalDateTime.now());

        return bundleRepository.save(bundle);
    }

    public void removeBundle(String idPsp, String idBundle) {
        Bundle bundle = getBundle(idBundle, idPsp);
        bundleRepository.delete(bundle);
    }

    private Bundle getBundle(String idBundle, String idPsp) {
        Optional<Bundle> bundle = bundleRepository.findById(idBundle);
        if (bundle.isEmpty()) {
            throw new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        }

        if (idPsp.compareTo(bundle.get().getIdPsp()) != 0) {
            throw new AppException(AppError.BUNDLE_PSP_CONFLICT, idBundle, idPsp);
        }

        return bundle.get();
    }

    public Bundles getBundlesByFiscalCode(String fiscalCode, Integer limit, Integer pageNumber) {
        var bundleList = ciBundleRepository
                .findByCiFiscalCode(fiscalCode)
                .parallelStream()
                .map(ciBundle -> bundleRepository.findById(ciBundle.getIdBundle()))
                .map(bundle -> modelMapper.map(bundle, BundleDetails.class))
                .collect(Collectors.toList());


        return Bundles.builder()
                .bundleDetailsList(bundleList)
                .build();
    }

    public BundleDetails getBundleByFiscalCode(String fiscalCode, String idBundle) {
        var ciBundle = ciBundleRepository.findByIdBundleAndCiFiscalCode(idBundle, fiscalCode)
                .orElseThrow(() -> new AppException(AppError.CI_BUNDLE_NOT_FOUND, idBundle, fiscalCode));

        var bundle = bundleRepository.findById(ciBundle.getIdBundle())
                .orElseThrow(() -> new AppException(AppError.BUNDLE_NOT_FOUND, idBundle));

        return modelMapper.map(bundle, BundleDetails.class);
    }

    public BundleDetailsAttributes getBundleAttributesByFiscalCode(String fiscalCode, String idBundle) {
        var ciBundle = ciBundleRepository.findByIdBundleAndCiFiscalCode(idBundle, fiscalCode)
                .orElseThrow(() -> new AppException(AppError.CI_BUNDLE_NOT_FOUND, idBundle, fiscalCode));

        return modelMapper.map(ciBundle, BundleDetailsAttributes.class);
    }

    public Object createBundleAttributesByCi(String fiscalCode, String idBundle, CiBundleAttribute bundleAttribute) {
        var ciBundle = ciBundleRepository.findByIdBundleAndCiFiscalCode(idBundle, fiscalCode)
                .orElseThrow(() -> new AppException(AppError.CI_BUNDLE_NOT_FOUND, idBundle, fiscalCode));
        modelMapper.map(bundleAttribute, CiBundleAttribute.class);
//        ciBundle.getAttributes().add();
        ciBundleRepository.save(ciBundle);

    }

}
