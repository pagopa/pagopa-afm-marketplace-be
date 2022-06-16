package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
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
public class BundleOfferService {

    @Autowired
    BundleRepository bundleRepository;

    @Autowired
    BundleOfferRepository bundleOfferRepository;

    @Autowired
    ModelMapper modelMapper;

//    public BundleOffers getPspOffers(String idPsp, Integer limit, Integer pageNumber) {
//        Pageable pageable = PageRequest.of(pageNumber, limit);
//        List<it.pagopa.afm.marketplacebe.model.offer.BundleOffer> bundleOfferList = new ArrayList<>();
//        Page<BundleOffer> page = bundleOfferRepository.findByIdPsp(idPsp, pageable);
//
//        return BundleOffers.builder()
//                .offers(getBundleOfferList(page))
//                .pageInfo(CommonUtil.buildPageInfo(page))
//                .build();
//    }

    public BundleOffers getPspOffers(String idPsp) {
        List<it.pagopa.afm.marketplacebe.model.offer.BundleOffer> bundleOfferList = bundleOfferRepository.findByIdPsp(idPsp)
                .stream()
                .map(bo -> modelMapper.map(bo, it.pagopa.afm.marketplacebe.model.offer.BundleOffer.class))
                .collect(Collectors.toList());

        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(bundleOfferList.size())
                .totalPages(1)
                .build();

        return BundleOffers.builder()
                .offers(bundleOfferList)
                .pageInfo(pageInfo)
                .build();
    }


    public List<BundleOffered> sendBundleOffer(String idPsp, String idBundle, CiFiscalCodeList ciFiscalCodeList) {
        // TODO verify idPsp and fiscal code

        Bundle bundle = getBundle(idBundle, idPsp);

        // verify bundle is private
        if (!bundle.getType().equals(BundleType.PRIVATE)) {
            throw new AppException(AppError.BUNDLE_OFFER_CONFLICT, idBundle, "Type not private");
        }

        List<BundleOffered> bundleOfferedList = new ArrayList<>();
        ciFiscalCodeList.getCiFiscalCodeList().forEach(fiscalCode -> {
            BundleOffer bundleOffer = BundleOffer.builder()
                    .ciFiscalCode(fiscalCode)
                    .idPsp(idPsp)
                    .idBundle(bundle.getId())
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

        Optional<BundleOffer> bundleOffer = bundleOfferRepository.findById(idBundleOffer);

        if (bundleOffer.isEmpty()) {
            throw new AppException(AppError.BUNDLE_OFFER_NOT_FOUND, idBundleOffer);
        }

        if (!bundleOffer.get().getIdPsp().equals(idPsp) || !bundleOffer.get().getIdBundle().equals(idBundle)) {
            throw new AppException(AppError.BUNDLE_OFFER_BAD_REQUEST, idBundle, String.format("idPSP=%s", idPsp));
        }

        bundleOfferRepository.delete(bundleOffer.get());
    }

    private Bundle getBundle(String idBundle, String idPsp) {
        Optional<Bundle> optBundle = bundleRepository.findById(idBundle, new PartitionKey(idPsp));
        if (optBundle.isEmpty()) {
            throw new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        }
        return optBundle.get();
    }
}
