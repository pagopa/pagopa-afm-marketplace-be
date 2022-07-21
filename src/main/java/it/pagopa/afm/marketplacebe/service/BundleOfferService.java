package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.*;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.offer.*;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.util.CommonUtil;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class BundleOfferService {

    public static final String ALREADY_DELETED = "Bundle has been deleted.";
    @Autowired
    BundleRepository bundleRepository;

    @Autowired
    BundleOfferRepository bundleOfferRepository;

    @Autowired
    CiBundleRepository ciBundleRepository;

    @Autowired
    ArchivedBundleOfferRepository archivedBundleOfferRepository;

    @Autowired
    ModelMapper modelMapper;


    public BundleOffers getPspOffers(String idPsp) {
        List<PspBundleOffer> bundleOfferList = bundleOfferRepository.findByIdPsp(idPsp)
                .stream()
                .map(bo -> modelMapper.map(bo, PspBundleOffer.class))
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
        Bundle bundle = getBundle(idBundle, idPsp);

        // verify if validityDateTo is after now
        if (!CommonUtil.isValidityDateToAcceptable(bundle.getValidityDateTo())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        // verify bundle is private
        if (!bundle.getType().equals(BundleType.PRIVATE)) {
            throw new AppException(AppError.BUNDLE_OFFER_CONFLICT, idBundle, "Type not private");
        }

        ciFiscalCodeList.getCiFiscalCodeList().forEach(fiscalCode -> {
            // check if the bundle has already been offered
            var ciBundle = ciBundleRepository.findByIdBundleAndCiFiscalCode(idBundle, fiscalCode);
            var offerBundle = bundleOfferRepository.findByIdBundleAndCiFiscalCodeAndAcceptedDateIsNullAndRejectionDateIsNull(idBundle, fiscalCode);
            if (ciBundle.isPresent() || offerBundle.isPresent()) {
                throw new AppException(AppError.BUNDLE_OFFER_CONFLICT, idBundle, "Bundle already offered to CI " + fiscalCode);
            }
        });

        List<BundleOffered> bundleOfferedList = new ArrayList<>();
        ciFiscalCodeList.getCiFiscalCodeList().forEach(fiscalCode -> {
            // add offer
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

        archiveBundleOffer(bundleOffer.get(), null);
    }

    public BundleCiOffers getCiOffers(String ciFiscalCode, String idPsp) {

        List<BundleOffer> offerList = idPsp == null ? bundleOfferRepository.findByCiFiscalCode(ciFiscalCode) : bundleOfferRepository.findByIdPsp(idPsp, new PartitionKey(ciFiscalCode));
        List<CiBundleOffer> bundleOfferList = offerList
                .stream()
                .map(bo -> modelMapper.map(bo, CiBundleOffer.class))
                .collect(Collectors.toList());

        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(bundleOfferList.size())
                .totalPages(1)
                .build();

        return BundleCiOffers.builder()
                .offers(bundleOfferList)
                .pageInfo(pageInfo)
                .build();
    }

    public CiBundleId acceptOffer(String ciFiscalCode, String idBundleOffer) {
        BundleOffer offer = getBundleOffer(idBundleOffer, ciFiscalCode);

        Bundle bundle = getBundle(offer.getIdBundle(), offer.getIdPsp());

        // verify if validityDateTo is after now
        if (!CommonUtil.isValidityDateToAcceptable(bundle.getValidityDateTo())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        // check if the bundle has already been offered
        var offerBundle = bundleOfferRepository.findByIdBundleAndCiFiscalCodeAndAcceptedDateIsNullAndRejectionDateIsNull(bundle.getId(), ciFiscalCode);
        if (offerBundle.isPresent()) {
            throw new AppException(AppError.BUNDLE_OFFER_CONFLICT, bundle.getId(), "Bundle already offered to CI " + ciFiscalCode);
        }

        Optional<CiBundle> ciBundle = ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(
                offer.getIdBundle(),
                ciFiscalCode
        );

        if (ciBundle.isPresent()) {
            throw new AppException(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED, idBundleOffer, offer.getAcceptedDate());
        } else {
            if (offer.getAcceptedDate() == null && offer.getRejectionDate() == null) {
                archiveBundleOffer(offer, true);

                // create CI-Bundle relation
                return CiBundleId.builder()
                        .id(ciBundleRepository.save(buildCiBundle(offer, bundle)).getId())
                        .build();
            } else if (offer.getAcceptedDate() == null && offer.getRejectionDate() != null) {
                throw new AppException(AppError.BUNDLE_OFFER_ALREADY_REJECTED, idBundleOffer, offer.getRejectionDate());
            } else {
                throw new AppException(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED, idBundleOffer, offer.getAcceptedDate());
            }
        }
    }

    public void rejectOffer(String ciFiscalCode, String idBundleOffer) {
        BundleOffer entity = getBundleOffer(idBundleOffer, ciFiscalCode);

        Bundle bundle = getBundle(entity.getIdBundle(), entity.getIdPsp());

        // verify if validityDateTo is after now
        if (!CommonUtil.isValidityDateToAcceptable(bundle.getValidityDateTo())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        if (entity.getAcceptedDate() == null && entity.getRejectionDate() == null) {
            archiveBundleOffer(entity, false);
        } else if (entity.getAcceptedDate() == null && entity.getRejectionDate() != null) {
            throw new AppException(AppError.BUNDLE_OFFER_ALREADY_REJECTED, idBundleOffer, entity.getRejectionDate());
        } else {
            throw new AppException(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED, idBundleOffer, entity.getAcceptedDate());
        }
    }

    /**
     * @param idBundleOffer Bundle Offer identifier
     * @param ciFiscalCode  CI identifier
     * @return the entity if exist
     * @throws AppException if not found
     */
    private BundleOffer getBundleOffer(String idBundleOffer, String ciFiscalCode) {
        return bundleOfferRepository.findById(idBundleOffer, new PartitionKey(ciFiscalCode))
                .orElseThrow(() -> new AppException(AppError.BUNDLE_OFFER_NOT_FOUND, idBundleOffer));
    }

    private CiBundle buildCiBundle(BundleOffer entity, Bundle bundle) {
        LocalDate buildTime = LocalDate.now();

        if (bundle.getValidityDateFrom() == null) {
            return CiBundle.builder()
                    .ciFiscalCode(entity.getCiFiscalCode())
                    .idBundle(entity.getIdBundle())
                    .validityDateTo(bundle.getValidityDateTo())
                    .validityDateFrom(buildTime)
                    .build();
        } else {
            return CiBundle.builder()
                    .ciFiscalCode(entity.getCiFiscalCode())
                    .idBundle(entity.getIdBundle())
                    .validityDateTo(bundle.getValidityDateTo())
                    .validityDateFrom(bundle.getValidityDateFrom().isBefore(buildTime) ?
                            buildTime : bundle.getValidityDateFrom())
                    .build();
        }
    }

    /**
     * @param entity   the new offer that will be used to replace old CiBundle
     * @param bundle   the bundle connected with the offer
     * @param ciBundle the old CiBundle that will be replaced/invalidated
     * @return the entity if exist
     * @throws AppException if not found
     */
    private CiBundle invalidateAndBuildCiBundle(BundleOffer entity, Bundle bundle, CiBundle ciBundle) {
        LocalDate buildTime = LocalDate.now();
        ciBundle.setValidityDateTo(buildTime);
        ciBundleRepository.save(ciBundle);

        if (bundle.getValidityDateFrom() == null) {
            return CiBundle.builder()
                    .ciFiscalCode(entity.getCiFiscalCode())
                    .idBundle(entity.getIdBundle())
                    .validityDateTo(bundle.getValidityDateTo())
                    .validityDateFrom(buildTime)
                    .build();
        } else {
            return CiBundle.builder()
                    .ciFiscalCode(entity.getCiFiscalCode())
                    .idBundle(entity.getIdBundle())
                    .validityDateTo(bundle.getValidityDateTo())
                    .validityDateFrom(bundle.getValidityDateFrom().isBefore(buildTime.plusDays(1)) ?
                            buildTime.plusDays(1) : bundle.getValidityDateFrom())
                    .build();
        }
    }


    /**
     * Retrieve bundle
     *
     * @param idBundle bundle identifier
     * @param idPsp    PSP identifier
     * @return the bundle if present
     */
    private Bundle getBundle(String idBundle, String idPsp) {
        Optional<Bundle> optBundle = bundleRepository.findById(idBundle, new PartitionKey(idPsp));
        if (optBundle.isEmpty()) {
            throw new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        }
        return optBundle.get();
    }

    /**
     * Archives a bundle offer.
     * Create a new {@link ArchivedBundleOffer} entity and deletes the {@link BundleOffer} entity.
     *
     * @param bundleOffer an entity of {@link BundleOffer}
     * @param accepted    true = request is accepted, false = not accepted, null = deleted
     */
    private void archiveBundleOffer(BundleOffer bundleOffer, Boolean accepted) {
        var offerToArchive = bundleOffer;

        if (Boolean.TRUE.equals(accepted)) {
            offerToArchive = offerToArchive.toBuilder()
                    .acceptedDate(LocalDateTime.now())
                    .build();
        }
        if (Boolean.FALSE.equals(accepted)) {
            offerToArchive = offerToArchive.toBuilder()
                    .rejectionDate(LocalDateTime.now())
                    .build();
        }

        archivedBundleOfferRepository.save(modelMapper.map(offerToArchive, ArchivedBundleOffer.class));
        bundleOfferRepository.delete(bundleOffer);
    }

}
