package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.BundleOffered;
import it.pagopa.afm.marketplacebe.model.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
public class BundleOfferService {

    @Autowired
    BundleOfferRepository bundleOfferRepository;

    public Flux<BundleOffer> getPspOffers(String idPsp, Integer size, String cursor) {
        // TODO
        return bundleOfferRepository.findByIdPsp(idPsp);
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
}
