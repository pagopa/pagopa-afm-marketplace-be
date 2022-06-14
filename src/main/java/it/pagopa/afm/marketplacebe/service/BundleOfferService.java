package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.model.BundleOffered;
import it.pagopa.afm.marketplacebe.model.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

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
}
