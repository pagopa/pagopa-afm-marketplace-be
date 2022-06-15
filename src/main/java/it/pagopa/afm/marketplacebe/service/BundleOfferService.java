package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.util.CommonUtil;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
public class BundleOfferService {

    @Autowired
    BundleOfferRepository bundleOfferRepository;

    @Autowired
    ModelMapper modelMapper;

    public BundleOffers getPspOffers(String idPsp, Integer limit, Integer pageNumber) {
        Pageable pageable = PageRequest.of(pageNumber, pageNumber);
        List<it.pagopa.afm.marketplacebe.model.offer.BundleOffer> bundleOfferList = new ArrayList<>();
        Page<BundleOffer> page = bundleOfferRepository.findByIdPsp(idPsp, pageable);

        return BundleOffers.builder()
                .offers(getBundleOfferList(page))
                .pageInfo(CommonUtil.buildPageInfo(page))
                .build();
    }

    public List<BundleOffered> sendBundleOffer(String idPsp, String idBundle, CiFiscalCodeList ciFiscalCodeList) {
        // TODO verify idPsp acceptability
        // TODO verify idBundle acceptability
        // TODO verify fiscal code validity

        List<BundleOffered> bundleOfferedList = new ArrayList<>();
        ciFiscalCodeList.getCiFiscalCodeList().forEach(fiscalCode -> {
            BundleOffer bundleOffer = BundleOffer.builder()
                    .ciFiscalCode(fiscalCode)
                    .idPsp(idPsp)
                    .idBundle(idBundle)
                    .insertedDate(LocalDateTime.now())
                    .build();
            bundleOfferRepository.save(bundleOffer);

            BundleOffered bundleOffered = BundleOffered.builder()
                    .idBundleOffer(bundleOffer.getId())
                    .ciFiscalCode(fiscalCode)
                    .build();
            bundleOfferedList.add(bundleOffered);
        });

        return bundleOfferedList;
    }

    public void removeBundleOffer(String idPsp, String idBundle, String idBundleOffer) {
        // find bundle offer by id
        // check the related idPsp and idBundle
        // remove bundle offer

        bundleOfferRepository.findById(idBundleOffer)
                .switchIfEmpty(Mono.error(new AppException(AppError.BUNDLE_OFFER_NOT_FOUND, idBundleOffer)))
                .filter(Objects::nonNull)
                .flatMap(bo -> {
                    if (!bo.getIdPsp().equals(idPsp) || !bo.getIdBundle().equals(idBundle)) {
                        Mono.error(new AppException(AppError.BUNDLE_OFFER_NOT_FOUND, idBundleOffer));
                    }
                    return bundleOfferRepository.delete(bo).then(Mono.just(bo));
                });
    }

    /**
     * Extract bundle offer list from a page
     *
     * @param page bundle offer page
     * @return list of bundle offer
     */
    private List<it.pagopa.afm.marketplacebe.model.offer.BundleOffer> getBundleOfferList(Page<BundleOffer> page) {
        return page
                .stream()
                .filter(Objects::nonNull)
                .map(bo -> modelMapper.map(bo, it.pagopa.afm.marketplacebe.model.offer.BundleOffer.class))
                .collect(Collectors.toList());
    }
}
