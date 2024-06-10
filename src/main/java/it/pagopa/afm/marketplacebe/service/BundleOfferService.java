package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.ArchivedBundleOffer;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.offer.BundleCiOffers;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiBundleId;
import it.pagopa.afm.marketplacebe.model.offer.CiBundleOffer;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.offer.PspBundleOffer;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.CosmosRepository;
import it.pagopa.afm.marketplacebe.util.CommonUtil;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static it.pagopa.afm.marketplacebe.util.CommonUtil.calculateTotalPages;

@Service
public class BundleOfferService {

    public static final String ALREADY_DELETED = "Bundle has been deleted.";

    private final BundleRepository bundleRepository;

    private final BundleOfferRepository bundleOfferRepository;

    private final CiBundleRepository ciBundleRepository;

    private final ArchivedBundleOfferRepository archivedBundleOfferRepository;

    private final CosmosRepository cosmosRepository;

    private final ModelMapper modelMapper;

    @Autowired
    public BundleOfferService(
            BundleRepository bundleRepository,
            BundleOfferRepository bundleOfferRepository,
            CiBundleRepository ciBundleRepository,
            ArchivedBundleOfferRepository archivedBundleOfferRepository,
            CosmosRepository cosmosRepository,
            ModelMapper modelMapper
    ) {
        this.bundleRepository = bundleRepository;
        this.bundleOfferRepository = bundleOfferRepository;
        this.ciBundleRepository = ciBundleRepository;
        this.archivedBundleOfferRepository = archivedBundleOfferRepository;
        this.cosmosRepository = cosmosRepository;
        this.modelMapper = modelMapper;
    }

    public BundleOffers getPspOffers(String idPsp, String ciTaxCode, String idBundle, Integer limit, Integer page) {
        List<PspBundleOffer> bundleOfferList =
                this.bundleOfferRepository.findByIdPspAndFiscalCodeAndIdBundle(idPsp, ciTaxCode, idBundle, limit * page, limit).stream()
                        .map(bo -> this.modelMapper.map(bo, PspBundleOffer.class))
                        .toList();

        Integer totalItems = this.bundleOfferRepository.getTotalItemsFindByIdPspAndFiscalCodeAndIdBundle(idPsp, ciTaxCode, idBundle);
        int totalPages = calculateTotalPages(limit, totalItems);

        return BundleOffers.builder()
                .offers(bundleOfferList)
                .pageInfo(PageInfo.builder()
                        .page(page)
                        .limit(limit)
                        .totalItems(Long.valueOf(totalItems))
                        .totalPages(totalPages)
                        .build())
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

    /**
     * Retrieve the paginated list of private bundle's offers of the specified CI
     *
     * @param ciTaxCode  tax code of the creditor institution to which the offers are addressed
     * @param idPsp      id of the payment service provider that has created the offers (used for to filter out the result)
     * @param bundleName the bundle name (used to filter out the result)
     * @param limit      the size of the page
     * @param page       the number of the page
     * @return the paginated list of offers
     */
    public BundleCiOffers getCiOffers(String ciTaxCode, String idPsp, String bundleName, Integer limit, Integer page) {
        List<String> idBundles = null;
        if (StringUtils.isNotEmpty(bundleName)) {
            idBundles = this.cosmosRepository.getBundlesByNameAndPSPBusinessName(bundleName, null, BundleType.PRIVATE.name())
                    .parallelStream()
                    .map(Bundle::getId)
                    .toList();
        }

        List<CiBundleOffer> bundleOfferList =
                this.bundleOfferRepository.findByIdPspAndFiscalCodeAndIdBundles(idPsp, ciTaxCode, idBundles, limit * page, limit)
                        .parallelStream()
                        .map(bo -> modelMapper.map(bo, CiBundleOffer.class))
                        .toList();

        Integer totalItems = this.bundleOfferRepository.getTotalItemsFindByIdPspAndFiscalCodeAndIdBundles(idPsp, ciTaxCode, idBundles);
        int totalPages = calculateTotalPages(limit, totalItems);

        return BundleCiOffers.builder()
                .offers(bundleOfferList)
                .pageInfo(PageInfo.builder()
                        .page(page)
                        .limit(limit)
                        .itemsFound(bundleOfferList.size())
                        .totalItems(Long.valueOf(totalItems))
                        .totalPages(totalPages)
                        .build())
                .build();
    }

    /**
     * Accept the provided private bundle offer.
     * Verify if the offer and the provided attributes are valid and if so it creates the corresponding CIBundle
     *
     * @param ciFiscalCode creditor institution's tax code
     * @param idBundleOffer bundle offer's id
     * @param bundleAttributes bundle attributes specified by the creditor institution
     * @return the id of the created CIBundle
     */
    public CiBundleId acceptOffer(String ciFiscalCode, String idBundleOffer, List<CiBundleAttributeModel> bundleAttributes) {
        BundleOffer offer = getBundleOffer(idBundleOffer, ciFiscalCode);
        Bundle bundle = getBundle(offer.getIdBundle(), offer.getIdPsp());

        // verify if bundle is already deleted (validityDateTo is after now)
        if (!CommonUtil.isValidityDateToAcceptable(bundle.getValidityDateTo())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        // check if the bundle has already been offered to the same CI
        if (isBundleOfferDuplicated(ciFiscalCode, idBundleOffer, bundle)) {
            throw new AppException(AppError.BUNDLE_OFFER_CONFLICT, bundle.getId(), "Bundle already offered to CI " + ciFiscalCode);
        }

        // verify if the offer is already been accepted
        if (isOfferAlreadyAccepted(ciFiscalCode, offer)) {
            throw new AppException(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED, idBundleOffer, offer.getAcceptedDate());
        }

        if (offer.getAcceptedDate() == null && offer.getRejectionDate() == null) {
            archiveBundleOffer(offer, true);

            List<CiBundleAttribute> attributes = new ArrayList<>();
            if (bundleAttributes != null && !bundleAttributes.isEmpty()) {
                validateCIBundleAttributes(bundleAttributes, bundle);
                attributes = buildCiBundleAttributes(bundle.getId(), bundleAttributes);
            }

            // create CI-Bundle relation
            return CiBundleId.builder()
                    .id(this.ciBundleRepository.save(buildCiBundle(offer, bundle, attributes)).getId())
                    .build();
        } else if (offer.getAcceptedDate() == null) {
            throw new AppException(AppError.BUNDLE_OFFER_ALREADY_REJECTED, idBundleOffer, offer.getRejectionDate());
        } else {
            throw new AppException(AppError.BUNDLE_OFFER_ALREADY_ACCEPTED, idBundleOffer, offer.getAcceptedDate());
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

    private CiBundle buildCiBundle(BundleOffer bundleOffer, Bundle bundle, List<CiBundleAttribute> attributes) {
        LocalDate validityDateFrom = bundle.getValidityDateFrom();
        LocalDate buildTime = LocalDate.now();
        if (validityDateFrom == null || validityDateFrom.isBefore(buildTime)) {
            validityDateFrom = buildTime;
        }

        return CiBundle.builder()
                .ciFiscalCode(bundleOffer.getCiFiscalCode())
                .idBundle(bundleOffer.getIdBundle())
                .type(bundle.getType())
                .validityDateTo(bundle.getValidityDateTo())
                .validityDateFrom(validityDateFrom)
                .attributes(attributes)
                .build();
    }

    /**
     * Retrieve bundle
     *
     * @param idBundle bundle identifier
     * @param idPsp    PSP identifier
     * @return the bundle if present
     */
    private Bundle getBundle(String idBundle, String idPsp) {
        return this.bundleRepository.findById(idBundle, new PartitionKey(idPsp))
                .orElseThrow(() -> new AppException(AppError.BUNDLE_NOT_FOUND, idBundle));
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

        this.archivedBundleOfferRepository.save(modelMapper.map(offerToArchive, ArchivedBundleOffer.class));
        this.bundleOfferRepository.delete(bundleOffer);
    }

    private List<CiBundleAttribute> buildCiBundleAttributes(String idBundle, List<CiBundleAttributeModel> ciBundleAttributeModelList) {
        return ciBundleAttributeModelList.parallelStream()
                .map(attribute -> CiBundleAttribute.builder()
                        .id(String.format("%s-%s", idBundle, UUID.randomUUID()))
                        .insertedDate(LocalDateTime.now())
                        .maxPaymentAmount(attribute.getMaxPaymentAmount())
                        .transferCategory(attribute.getTransferCategory())
                        .transferCategoryRelation(attribute.getTransferCategoryRelation())
                        .build())
                .toList();
    }

    private void validateCIBundleAttributes(List<CiBundleAttributeModel> attributeModelList, Bundle bundle) {
        attributeModelList.parallelStream().forEach(
                attribute -> {
                    // rule R15: attribute payment amount should be lower than bundle one
                    if (attribute.getMaxPaymentAmount().compareTo(bundle.getPaymentAmount()) > 0) {
                        throw new AppException(
                                AppError.BUNDLE_OFFER_BAD_REQUEST,
                                bundle.getId(),
                                "Payment amount should be lower than or equal to bundle payment amount."
                        );
                    }
                    if (attribute.getTransferCategory() == null && attributeModelList.size() > 1) {
                        throw new AppException(
                                AppError.BUNDLE_OFFER_BAD_ATTRIBUTE,
                                bundle.getId(),
                                "Only one attribute can be specified if the attribute has transfer category null"
                        );
                    }

                });
    }

    private boolean isOfferAlreadyAccepted(String ciFiscalCode, BundleOffer offer) {
        return this.ciBundleRepository
                .findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(offer.getIdBundle(), ciFiscalCode)
                .isPresent();
    }

    private boolean isBundleOfferDuplicated(String ciFiscalCode, String idBundleOffer, Bundle bundle) {
        var offerBundle = this.bundleOfferRepository
                .findByIdBundleAndCiFiscalCodeAndAcceptedDateIsNullAndRejectionDateIsNull(bundle.getId(), ciFiscalCode);
        return offerBundle.isPresent() && !offerBundle.get().getId().equals(idBundleOffer);
    }
}
