package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.*;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.bundle.BundleResponse;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleDetails;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
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
        List<it.pagopa.afm.marketplacebe.model.bundle.Bundle> bundleList = bundleRepository
                .findByIdPsp(idPsp)
                .stream()
                .map(bundle -> modelMapper.map(bundle, it.pagopa.afm.marketplacebe.model.bundle.Bundle.class))
                .collect(Collectors.toList());

        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(bundleList.size())
                .totalPages(1)
                .build();

        return Bundles.builder().bundleList(bundleList).pageInfo(pageInfo).build();
    }

    public it.pagopa.afm.marketplacebe.model.bundle.Bundle getBundleById(String idBundle, String idPsp) {
        Bundle bundle = getBundle(idBundle, idPsp);

        return modelMapper.map(bundle, it.pagopa.afm.marketplacebe.model.bundle.Bundle.class);
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
    public CiFiscalCodeList getCIs(String idBundle, String idPSP){
       List<CiBundle> subscriptions =  ciBundleRepository.findByIdBundle(idBundle);
       List<String> CIs = new ArrayList<>();
       CiFiscalCodeList ciFiscalCodeList = new CiFiscalCodeList();

       for(CiBundle ciBundle: subscriptions){
           if (!checkCiBundle(ciBundle, idPSP)){
               throw new AppException(AppError.BUNDLE_PSP_CONFLICT, idBundle, idPSP);
           }
           CIs.add(ciBundle.getCiFiscalCode());
       }
       ciFiscalCodeList.setCiFiscalCodeList(CIs);

       return ciFiscalCodeList;
    }

    public CiBundleDetails getCIDetails(String idBundle, String idPsp, String ciFiscalCode){
        Optional<CiBundle> ciBundle = ciBundleRepository.findByIdBundleAndCiFiscalCode(idBundle, ciFiscalCode);

        if(ciBundle.isEmpty()){
            throw new AppException(AppError.CI_BUNDLE_NOT_FOUND, idBundle, ciFiscalCode);
        }

        getBundle(idBundle, idPsp);

        return CiBundleDetails.builder()
                .validityDateTo(ciBundle.get().getValidityDateTo())
                .attributes(ciBundle.get().getAttributes().stream().map(
                        attribute -> modelMapper.map(
                                attribute, it.pagopa.afm.marketplacebe.model.bundle.CiBundleAttribute.class)
                ).collect(Collectors.toList()))
                .build();
    }

    private boolean checkCiBundle(CiBundle ciBundle, String idPSP){
        return !bundleRepository.findById(ciBundle.getIdBundle()).isEmpty() ||
                bundleRepository.findById(ciBundle.getIdBundle()).get().getIdPsp() != idPSP;
    }
}
