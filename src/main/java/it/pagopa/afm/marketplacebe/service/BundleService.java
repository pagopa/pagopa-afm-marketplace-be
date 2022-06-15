package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.entity.Touchpoint;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.bundle.BundleResponse;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class BundleService {

    @Autowired
    private BundleRepository bundleRepository;

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


    public BundleResponse createBundle(String idPsp, BundleRequest bundleRequest) {
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
                .validityDateFrom(bundleRequest.getValidityDateFrom())
                .validityDateTo(bundleRequest.getValidityDateTo())
                .insertedDate(now)
                .lastUpdatedDate(now)
                .build();

        return BundleResponse.builder().idBundle(bundleRepository.save(bundle).getIdBundle()).build();
    }

    /*
    public Mono<Void> deleteBundle(String idBundle){
        return bundleRepository.deleteById(idBundle);
    }

    public Mono<Bundle> updateBundle(Bundle bundle){
        return bundleRepository.save(bundle);
    }
     */
}
