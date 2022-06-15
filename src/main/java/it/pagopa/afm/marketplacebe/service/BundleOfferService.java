package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class BundleOfferService {

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

        Optional<BundleOffer> bundleOffer = bundleOfferRepository.findById(idBundleOffer);

        if (bundleOffer.isEmpty()) {
            throw new AppException(AppError.BUNDLE_OFFER_NOT_FOUND, idBundleOffer);
        }

        if (!bundleOffer.get().getIdPsp().equals(idPsp) || !bundleOffer.get().getIdBundle().equals(idBundle)) {
            throw new AppException(AppError.BUNDLE_OFFER_BAD_REQUEST, String.format("idPSP=%s idBundle=%s", idPsp, idBundle));
        }

        bundleOfferRepository.delete(bundleOffer.get());
    }
}
