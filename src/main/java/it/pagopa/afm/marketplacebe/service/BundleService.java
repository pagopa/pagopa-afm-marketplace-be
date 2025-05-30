package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.entity.*;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.*;
import it.pagopa.afm.marketplacebe.model.offer.BundleCreditorInstitutionResource;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.repository.*;
import it.pagopa.afm.marketplacebe.task.*;
import it.pagopa.afm.marketplacebe.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;

import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.StreamSupport;

import static it.pagopa.afm.marketplacebe.util.CommonUtil.calculateTotalPages;

@Service
@Slf4j
public class BundleService {

    public static final String ALREADY_DELETED = "Bundle has been deleted.";

    private final BundleRepository bundleRepository;

    private final CiBundleRepository ciBundleRepository;

    private final BundleRequestRepository bundleRequestRepository;

    private final BundleOfferRepository bundleOfferRepository;

    private final TouchpointRepository touchpointRepository;

    private final ArchivedBundleRepository archivedBundleRepository;

    private final ArchivedBundleOfferRepository archivedBundleOfferRepository;

    private final ArchivedBundleRequestRepository archivedBundleRequestRepository;

    private final ArchivedCiBundleRepository archivedCiBundleRepository;

    private final ValidBundleRepository validBundleRepository;

    private final PaymentTypeRepository paymentTypeRepository;

    private final CosmosRepository cosmosRepository;

    private final ModelMapper modelMapper;

    @Autowired
    public BundleService(
            BundleRepository bundleRepository,
            CiBundleRepository ciBundleRepository,
            BundleRequestRepository bundleRequestRepository,
            BundleOfferRepository bundleOfferRepository,
            TouchpointRepository touchpointRepository,
            ArchivedBundleRepository archivedBundleRepository,
            ArchivedBundleOfferRepository archivedBundleOfferRepository,
            ArchivedBundleRequestRepository archivedBundleRequestRepository,
            ArchivedCiBundleRepository archivedCiBundleRepository,
            ValidBundleRepository validBundleRepository,
            PaymentTypeRepository paymentTypeRepository,
            CosmosRepository cosmosRepository,
            ModelMapper modelMapper
    ) {
        this.bundleRepository = bundleRepository;
        this.ciBundleRepository = ciBundleRepository;
        this.bundleRequestRepository = bundleRequestRepository;
        this.bundleOfferRepository = bundleOfferRepository;
        this.touchpointRepository = touchpointRepository;
        this.archivedBundleRepository = archivedBundleRepository;
        this.archivedBundleOfferRepository = archivedBundleOfferRepository;
        this.archivedBundleRequestRepository = archivedBundleRequestRepository;
        this.archivedCiBundleRepository = archivedCiBundleRepository;
        this.validBundleRepository = validBundleRepository;
        this.paymentTypeRepository = paymentTypeRepository;
        this.cosmosRepository = cosmosRepository;
        this.modelMapper = modelMapper;
    }

    /**
     * Retrieve the paginated list of bundles given the type, name and valid date
     *
     * @param bundleTypes list of bundle's type
     * @param name        bundle's name
     * @param validFrom   validity date of bundles, used to retrieve all bundles valid from the specified date
     * @param expireAt    validity date of bundles, used to retrieve all bundles that expire at the specified date
     * @param limit       page size
     * @param pageNumber  page number
     * @return a paginated list of bundles
     */
    public Bundles getBundles(
            List<BundleType> bundleTypes,
            String name,
            LocalDate validFrom,
            LocalDate expireAt,
            Integer limit,
            Integer pageNumber
    ) {
        // NOT a search by idPsp --> return only valid bundles
        List<PspBundleDetails> bundleList = this.cosmosRepository
                .getBundlesByNameAndTypeAndValidityDateFromAndExpireAt(name, bundleTypes, validFrom, expireAt, limit * pageNumber, limit)
                .stream()
                .map(bundle -> this.modelMapper.map(bundle, PspBundleDetails.class))
                .toList();

        Long totalItems = this.cosmosRepository.getTotalItemsFindByNameAndTypeAndValidityDateFromAndExpireAt(name, bundleTypes, validFrom, expireAt);
        int totalPages = calculateTotalPages(limit, totalItems);

        return Bundles.builder()
                .bundleDetailsList(bundleList)
                .pageInfo(PageInfo.builder()
                        .limit(limit)
                        .page(pageNumber)
                        .itemsFound(bundleList.size())
                        .totalItems(totalItems)
                        .totalPages(totalPages)
                        .build())
                .build();
    }

    public Bundles getBundlesByIdPsp(
            String idPsp,
            List<BundleType> bundleTypes,
            String name,
            Sort.Direction maxPaymentAmountOrder,
            Long paymentAmountMinRange,
            Long paymentAmountMaxRange,
            LocalDate validBefore,
            LocalDate validAfter,
            LocalDate expireBefore,
            LocalDate expireAfter,
            Integer pageNumber,
            Integer pageSize
    ) {
        // Search by idPsp --> return all bundles
        List<PspBundleDetails> bundleList = getBundlesByNameAndType(idPsp, name, bundleTypes, maxPaymentAmountOrder, paymentAmountMinRange, paymentAmountMaxRange, validBefore, validAfter, expireBefore, expireAfter, pageSize, pageNumber)
                .stream()
                .map(bundle -> this.modelMapper.map(bundle, PspBundleDetails.class))
                .toList();

        Long totalItems = this.cosmosRepository.getTotalItems(idPsp, name, bundleTypes, paymentAmountMinRange, paymentAmountMaxRange, validBefore, validAfter, expireBefore, expireAfter);
        int totalPages = calculateTotalPages(pageSize, totalItems);

        return Bundles.builder()
                .bundleDetailsList(bundleList)
                .pageInfo(PageInfo.builder()
                        .itemsFound(bundleList.size())
                        .limit(pageSize)
                        .page(pageNumber)
                        .totalPages(totalPages)
                        .totalItems(totalItems)
                        .build())
                .build();
    }

    public PspBundleDetails getBundleById(String idBundle, String idPsp) {
        Bundle bundle = getBundle(idBundle, idPsp);

        return modelMapper.map(bundle, PspBundleDetails.class);
    }

    /**
     * Retrieve the detail of the bundle with the provided id
     *
     * @param idBundle bundle id
     * @return the bundle details
     */
    public PspBundleDetails getBundleDetailsById(String idBundle) {
        Bundle bundle = getBundle(idBundle);

        return modelMapper.map(bundle, PspBundleDetails.class);
    }

    public List<BundleResponse> createBundleByList(String idPsp, List<BundleRequest> bundleRequestList) {
        List<Bundle> bundles = new ArrayList<>();
        for (BundleRequest bundleRequest : ListUtils.emptyIfNull(bundleRequestList)) {
            var bundle = generateBundle(idPsp, bundleRequest);
            bundles.add(bundle);
        }

        return StreamSupport.stream(bundleRepository.saveAll(bundles).spliterator(), true)
                .map(b -> BundleResponse.builder().idBundle(b.getId()).build()).toList();
    }

    public BundleResponse createBundle(String idPsp, BundleRequest bundleRequest) {
        var bundle = generateBundle(idPsp, bundleRequest);
        bundleRepository.save(bundle);
        return BundleResponse.builder()
                .idBundle(bundle.getId())
                .build();
    }

    private Bundle generateBundle(String idPsp, BundleRequest bundleRequest) {
        setVerifyTouchpointAnyIfNull(bundleRequest);

        // verify validityDateFrom, if null set to now +1d
        bundleRequest.setValidityDateFrom(getNextAcceptableDate(bundleRequest.getValidityDateFrom()));

        // verify validityDateFrom and validityDateTo
        analyzeValidityDate(bundleRequest, null);

        // verify minPaymentAmount < maxPaymentAmount
        analyzeMinMaxPaymentAmount(bundleRequest);

        // check if exists already the same configuration (minPaymentAmount, maxPaymentAmount, paymentType, touchpoint, type, transferCategoryList)
        // if it exists check validityDateFrom of the new configuration is next to validityDateTo of the existing one
        // check if the same payment amount range must not have the same tuple (paymentType, touchpoint, type, transferCategoryList)
        // check if there is overlapping transferCategoryList
        analyzeBundlesOverlapping(null, idPsp, bundleRequest);

        // verify if paymentType exists with the requested name
        Optional.ofNullable(bundleRequest.getPaymentType()).ifPresent(value -> getPaymentTypeByName(bundleRequest.getPaymentType()));

        LocalDateTime now = LocalDateTime.now();
        return Bundle.builder()
                .idPsp(idPsp)
                .idChannel(bundleRequest.getIdChannel())
                .idBrokerPsp(bundleRequest.getIdBrokerPsp())
                .cart(bundleRequest.getCart())
                .idCdi(bundleRequest.getIdCdi())
                .abi(bundleRequest.getAbi())
                .name(bundleRequest.getName())
                .pspBusinessName(bundleRequest.getPspBusinessName())
                .urlPolicyPsp(bundleRequest.getUrlPolicyPsp())
                .description(bundleRequest.getDescription())
                .paymentAmount(bundleRequest.getPaymentAmount())
                .minPaymentAmount(bundleRequest.getMinPaymentAmount())
                .maxPaymentAmount(bundleRequest.getMaxPaymentAmount())
                .paymentType(bundleRequest.getPaymentType())
                .digitalStamp(CommonUtil.deNull(bundleRequest.getDigitalStamp()))
                .digitalStampRestriction(CommonUtil.deNull(bundleRequest.getDigitalStamp()) && CommonUtil.deNull(bundleRequest.getDigitalStampRestriction()))
                .touchpoint(bundleRequest.getTouchpoint())
                .type(bundleRequest.getType())
                .transferCategoryList(bundleRequest.getTransferCategoryList())
                .validityDateFrom(bundleRequest.getValidityDateFrom())
                .validityDateTo(bundleRequest.getValidityDateTo())
                .insertedDate(now)
                .lastUpdatedDate(now)
                .onUs(bundleRequest.getOnUs())
                .build();
    }

    public Bundle updateBundle(String idPsp, String idBundle, BundleRequest bundleRequest, boolean forceUpdate) {
        setVerifyTouchpointAnyIfNull(bundleRequest);

        Bundle bundle = getBundle(idBundle, idPsp);

        // verify validityDateFrom, if it is null set to now +1d
        bundleRequest.setValidityDateFrom(getNextAcceptableDate(bundleRequest.getValidityDateFrom()));

        // verify maxPaymentAmount > minPaymentAmount
        analyzeMinMaxPaymentAmount(bundleRequest);

        if (!forceUpdate) {
            // verify validityDateFrom and validityDateTo
            analyzeValidityDate(bundleRequest, bundle);
        }

        // check if exists already the same configuration (minPaymentAmount, maxPaymentAmount, paymentType, touchpoint, type, transferCategoryList)
        // if it exists check validityDateFrom of the new configuration is next to validityDateTo of the existing one
        // check if the same payment amount range must not have the same tuple (paymentType, touchpoint, type, transferCategoryList)
        // check if there is overlapping transferCategoryList
        analyzeBundlesOverlapping(idBundle, idPsp, bundleRequest);

        bundle.setIdChannel(bundleRequest.getIdChannel());
        bundle.setIdBrokerPsp(bundleRequest.getIdBrokerPsp());
        bundle.setCart(bundleRequest.getCart());
        bundle.setIdCdi(bundleRequest.getIdCdi());
        bundle.setAbi(bundleRequest.getAbi());
        bundle.setName(bundleRequest.getName());
        bundle.setPspBusinessName(bundleRequest.getPspBusinessName());
        bundle.setUrlPolicyPsp(bundleRequest.getUrlPolicyPsp());
        bundle.setDescription(bundleRequest.getDescription());
        bundle.setPaymentAmount(bundleRequest.getPaymentAmount());
        bundle.setMinPaymentAmount(bundleRequest.getMinPaymentAmount());
        bundle.setMaxPaymentAmount(bundleRequest.getMaxPaymentAmount());
        bundle.setPaymentAmount(bundleRequest.getPaymentAmount());
        bundle.setPaymentType(bundleRequest.getPaymentType());
        bundle.setTouchpoint(bundleRequest.getTouchpoint());
        bundle.setTransferCategoryList(bundleRequest.getTransferCategoryList());
        bundle.setValidityDateFrom(bundleRequest.getValidityDateFrom());
        bundle.setValidityDateTo(bundleRequest.getValidityDateTo());
        bundle.setLastUpdatedDate(LocalDateTime.now());
        bundle.setDigitalStamp(CommonUtil.deNull(bundleRequest.getDigitalStamp()));
        bundle.setDigitalStampRestriction(CommonUtil.deNull(bundleRequest.getDigitalStamp()) && CommonUtil.deNull(bundleRequest.getDigitalStampRestriction()));
        bundle.setType(bundleRequest.getType());
        bundle.setOnUs(bundleRequest.getOnUs());

        // rule R15: adapt paymentAmount of the related ciBundle
        List<CiBundle> ciBundles = ciBundleRepository.findByIdBundle(bundle.getId());
        ciBundles.parallelStream().forEach(ciBundle ->
                ciBundle.getAttributes().parallelStream().forEach(attribute -> {
                    if (attribute.getMaxPaymentAmount() > bundle.getPaymentAmount()) {
                        attribute.setMaxPaymentAmount(bundle.getPaymentAmount());
                        ciBundleRepository.save(ciBundle);
                    }
                })
        );

        return bundleRepository.save(bundle);
    }

    /**
     * Mark the specified bundle as expired by setting its validityDateTo field to today. Then updates all linked ciBundles
     * as expired and deletes all bundle request/offer that have not been already accepted or rejected.
     *
     * @param idPsp    payment service provider's id
     * @param idBundle bundle's id
     */
    public void removeBundle(String idPsp, String idBundle) {
        Bundle bundle = getBundle(idBundle, idPsp);

        if (bundle.getValidityDateTo() != null && LocalDate.now().plusDays(1).isAfter(bundle.getValidityDateTo())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, "Bundle has been already deleted.");
        }

        // set validityDateTo=now in order to invalidate the bundle (logical delete)
        bundle.setValidityDateTo(LocalDate.now());
        this.bundleRepository.save(bundle);

        // invalidate all references
        // ci-bundles
        List<CiBundle> ciBundleList = this.ciBundleRepository.findByIdBundle(idBundle);
        ciBundleList.forEach(ciBundle -> ciBundle.setValidityDateTo(LocalDate.now()));
        this.ciBundleRepository.saveAll(ciBundleList);

        // bundle requests (if not accepted/rejected can be deleted physically)
        List<BundleRequestEntity> requests = this.bundleRequestRepository.findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(idBundle, idPsp);
        this.bundleRequestRepository.deleteAll(requests);

        // bundle offers (if not accepted/rejected can be deleted physically)
        List<BundleOffer> offers = this.bundleOfferRepository.findByIdPspAndIdBundleAndAcceptedDateIsNullAndRejectionDateIsNull(idPsp, idBundle);
        this.bundleOfferRepository.deleteAll(offers);
    }

    /**
     * Retrieve the paginated list of Creditor Institution subscribed to a public bundle
     *
     * @param idBundle     public bundle id
     * @param idPSP        PSP's code
     * @param ciFiscalCode Creditor Institution's tax code
     * @param limit        number of element in one page
     * @param pageNumber   page number
     * @return the paginated list of Creditor Institution's tax codes
     */
    public BundleCreditorInstitutionResource getCIs(
            String idBundle,
            String idPSP,
            @Nullable String ciFiscalCode,
            Integer limit,
            Integer pageNumber
    ) {
        List<CiBundle> subscriptions = ciBundleRepository.findByIdBundleAndCiFiscalCode(idBundle, ciFiscalCode, limit * pageNumber, limit);

        List<CiBundleDetails> ciBundleDetails = subscriptions.parallelStream()
                .map(ciBundle -> {
                    if (!checkCiBundle(ciBundle, idPSP)) {
                        throw new AppException(AppError.BUNDLE_PSP_CONFLICT, idBundle, idPSP);
                    }
                    return modelMapper.map(ciBundle, CiBundleDetails.class);
                })
                .toList();

        Integer totalItems = ciBundleRepository.getTotalItemsFindByIdBundleAndCiFiscalCode(idBundle, ciFiscalCode);
        int totalPages = calculateTotalPages(limit, totalItems);

        return BundleCreditorInstitutionResource.builder()
                .ciBundleDetails(ciBundleDetails)
                .pageInfo(PageInfo.builder()
                        .page(pageNumber)
                        .limit(limit)
                        .totalPages(totalPages)
                        .totalItems(Long.valueOf(totalItems))
                        .build())
                .build();
    }

    /**
     * Retrieve the subscription details between the specified creditor institution and bundle
     *
     * @param idBundle     bundle's id
     * @param idPsp        payment service provider's id
     * @param ciFiscalCode creditor institution's tax code
     * @return the details of the subscription
     */
    public CiBundleDetails getCIDetails(String idBundle, String idPsp, String ciFiscalCode) {
        Bundle bundle = getBundle(idBundle, idPsp);

        Optional<CiBundle> optionalCIBundle = ciBundleRepository.findByIdBundleAndCiFiscalCode(bundle.getId(), ciFiscalCode);

        if (optionalCIBundle.isEmpty()) {
            throw new AppException(AppError.CI_BUNDLE_NOT_FOUND, idBundle, ciFiscalCode);
        }

        CiBundle ciBundle = optionalCIBundle.get();
        return modelMapper.map(ciBundle, CiBundleDetails.class);
    }

    /**
     * Retrieve the paginated list of information about the relation between a bundle and a creditor institution
     *
     * @param taxCode         creditor institution's tax code
     * @param bundleType      the type of bundle,  used to filter out the result
     * @param bundleName      the bundle name, used to filter out the result
     * @param pspBusinessName payment service provider's business name, used to filter out the result
     * @param limit           the number of element in the page
     * @param pageNumber      the page number
     * @return the paginated list the details about the relation between a bundle and a creditor institution
     */
    public CiBundles getBundlesByFiscalCode(
            @NotNull String taxCode,
            BundleType bundleType,
            String bundleName,
            String pspBusinessName,
            Integer limit,
            Integer pageNumber
    ) {
        List<String> idBundles = null;
        String type = bundleType != null ? bundleType.name() : null;
        if (StringUtils.isNotEmpty(pspBusinessName) || StringUtils.isNotEmpty(bundleName)) {
            idBundles = this.cosmosRepository.getBundlesByNameAndPSPBusinessName(bundleName, pspBusinessName, type)
                    .parallelStream()
                    .map(Bundle::getId)
                    .toList();
        }

        var bundleList = this.ciBundleRepository
                .findByCiFiscalCodeAndTypeAndIdBundles(taxCode, type, idBundles, limit * pageNumber, limit)
                .parallelStream()
                .map(ciBundle -> this.modelMapper.map(ciBundle, CiBundleDetails.class))
                .toList();

        Integer totalItems = this.ciBundleRepository
                .getTotalItemsFindByCiFiscalCodeAndTypeAndIdBundles(taxCode, type, idBundles);
        int totalPages = calculateTotalPages(limit, totalItems);

        return CiBundles.builder()
                .bundleDetailsList(bundleList)
                .pageInfo(PageInfo.builder()
                        .limit(limit)
                        .page(pageNumber)
                        .itemsFound(bundleList.size())
                        .totalItems(Long.valueOf(totalItems))
                        .totalPages(totalPages)
                        .build())
                .build();
    }

    /**
     * Retrieve the information about the relation between a bundle and a creditor institution
     *
     * @param fiscalCode creditor institution's tax code
     * @param idBundle   bundle's id
     * @return the details about the relation between a bundle and a creditor institution
     */
    public CiBundleDetails getBundleByFiscalCode(@NotNull String fiscalCode, @NotNull String idBundle) {
        var ciBundle = this.ciBundleRepository.findByIdBundleAndCiFiscalCode(idBundle, fiscalCode)
                .orElseThrow(() -> new AppException(AppError.CI_BUNDLE_NOT_FOUND, idBundle, fiscalCode));
        return this.modelMapper.map(ciBundle, CiBundleDetails.class);
    }

    public void removeBundleByFiscalCode(@NotNull String fiscalCode, @NotNull String idCiBundle) {
        CiBundle ciBundle = ciBundleRepository.findById(idCiBundle, new PartitionKey(fiscalCode))
                .orElseThrow(() -> new AppException(AppError.CI_BUNDLE_ID_NOT_FOUND, idCiBundle));

        // set validityDateTo to now in order to be deleted by job at the next iteration
        ciBundle.setValidityDateTo(LocalDate.now());
        ciBundleRepository.save(ciBundle);
    }

    public BundleDetailsAttributes getBundleAttributesByFiscalCode(
            @NotNull String fiscalCode,
            @NotNull String idBundle
    ) {
        var ciBundle = findCiBundle(fiscalCode, idBundle);

        return modelMapper.map(ciBundle, BundleDetailsAttributes.class);
    }

    public BundleAttributeResponse createBundleAttributesByCi(
            @NotNull String fiscalCode,
            @NotNull String idBundle,
            @NotNull CiBundleAttributeModel bundleAttribute
    ) {
        // bundle attribute should be created only for global bundle
        // for public bundle CI should send a new request to PSP
        Bundle bundle = getBundle(idBundle);

        if (bundle.getValidityDateTo() != null && LocalDate.now().plusDays(1).isAfter(bundle.getValidityDateTo())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        if (bundle.getType().equals(BundleType.GLOBAL)) {
            throw new AppException(AppError.CI_BUNDLE_BAD_REQUEST, String.format("Bundle with id %s is not private or public.", idBundle));
        }

        // rule R15: attribute payment amount should be lower than bundle one
        if (bundleAttribute.getMaxPaymentAmount().compareTo(bundle.getPaymentAmount()) > 0) {
            throw new AppException(AppError.CI_BUNDLE_BAD_REQUEST, "Payment amount should be lower than or equal to bundle payment amount.");
        }

        // find or create CI-Bundle
        CiBundle ciBundle;
        try {
            ciBundle = findCiBundle(fiscalCode, bundle.getId());
        } catch (AppException e) {
            // create
            ciBundle = CiBundle.builder()
                    .ciFiscalCode(fiscalCode)
                    .idBundle(bundle.getId())
                    .insertedDate(LocalDateTime.now())
                    .attributes(new ArrayList<>())
                    .build();
            ciBundleRepository.save(ciBundle);
        }

        // create new attribute and add to CI-Bundle
        var attribute = modelMapper.map(bundleAttribute, CiBundleAttribute.class)
                .toBuilder()
                .id(idBundle + "-" + UUID.randomUUID()) // generate id
                .insertedDate(LocalDateTime.now())
                .build();

        if (ciBundle.getAttributes() != null) {
            ciBundle.getAttributes().add(attribute);
        } else {
            throw new AppException(AppError.BUNDLE_ATTRIBUTE_NOT_INITIALIZED, ciBundle.getId());
        }

        // save CI-Bundle with new attribute
        ciBundleRepository.save(ciBundle);

        return BundleAttributeResponse.builder()
                .idBundleAttribute(attribute.getId())
                .build();
    }

    public void updateBundleAttributesByCi(
            @NotNull String fiscalCode,
            @NotNull String idBundle,
            @NotNull String idAttribute,
            @NotNull CiBundleAttributeModel bundleAttribute
    ) {
        // bundle attribute should be updated only for global bundle
        // for public bundle CI should send a new request to PSP
        Bundle bundle = getBundle(idBundle);

        if (bundle.getValidityDateTo() != null && !bundle.getValidityDateTo().isAfter(LocalDate.now())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        if (!bundle.getType().equals(BundleType.GLOBAL)) {
            throw new AppException(AppError.CI_BUNDLE_BAD_REQUEST, String.format("Bundle with id %s is not global.", idBundle));
        }

        // rule R15: attribute payment amount should be lower than bundle one
        if (bundleAttribute.getMaxPaymentAmount().compareTo(bundle.getPaymentAmount()) > 0) {
            throw new AppException(AppError.CI_BUNDLE_BAD_REQUEST, "Payment amount should be lower than or equal to bundle payment amount.");
        }

        var ciBundle = findCiBundle(fiscalCode, idBundle);
        var attribute = ciBundle.getAttributes().parallelStream()
                .filter(elem -> idAttribute.equals(elem.getId()))
                .findFirst();

        if (attribute.isPresent()) {
            attribute.get().setMaxPaymentAmount(bundleAttribute.getMaxPaymentAmount());
            attribute.get().setTransferCategory(bundleAttribute.getTransferCategory());
            attribute.get().setTransferCategoryRelation(bundleAttribute.getTransferCategoryRelation());

            ciBundleRepository.save(ciBundle);
        } else {
            throw new AppException(AppError.BUNDLE_ATTRIBUTE_NOT_FOUND, idAttribute);
        }
    }

    public void removeBundleAttributesByCi(
            @NotNull String fiscalCode,
            @NotNull String idBundle,
            @NotNull String idAttribute
    ) {
        // bundle attribute should be removed only for global and public bundles
        Bundle bundle = getBundle(idBundle);

        if (bundle.getValidityDateTo() != null && !bundle.getValidityDateTo().isAfter(LocalDate.now())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        if (bundle.getType().equals(BundleType.PRIVATE)) {
            throw new AppException(AppError.CI_BUNDLE_BAD_REQUEST, String.format("Bundle with id %s is not global or public.", idBundle));
        }

        // find CI-Bundle
        var ciBundle = findCiBundle(fiscalCode, idBundle);

        // remove attribute from CI-Bundle if exists
        var result = ciBundle.getAttributes()
                .removeIf(attribute -> idAttribute.equals(attribute.getId()));
        if (!result) {
            throw new AppException(AppError.BUNDLE_ATTRIBUTE_NOT_FOUND, idAttribute);
        } else {
            ciBundleRepository.save(ciBundle);
            // if bundle is global and there are no attributes -> remove ci-bundle relationship
            // in order to maintain logical relationship instead of physical one
            if (bundle.getType().equals(BundleType.GLOBAL) && ciBundle.getAttributes().isEmpty()) {
                ciBundleRepository.delete(ciBundle);
            }
        }
    }

    public void getConfiguration() {
        log.info("Configuration requested..." + LocalDateTime.now());

        BundleTaskExecutor bundleArchiver = new BundleTaskExecutor(bundleRepository, archivedBundleRepository);
        BundleOfferTaskExecutor bundleOfferArchiver = new BundleOfferTaskExecutor(bundleOfferRepository, archivedBundleOfferRepository);
        BundleRequestTaskExecutor bundleRequestArchiver = new BundleRequestTaskExecutor(bundleRequestRepository, archivedBundleRequestRepository);
        CiBundleTaskExecutor ciBundleArchiver = new CiBundleTaskExecutor(ciBundleRepository, archivedCiBundleRepository);
        ValidBundlesTaskExecutor validBundlesTaskExecutor = new ValidBundlesTaskExecutor(bundleRepository, ciBundleRepository, validBundleRepository);

        TaskManager taskManager = new TaskManager(
                bundleArchiver,
                bundleOfferArchiver,
                bundleRequestArchiver,
                ciBundleArchiver,
                validBundlesTaskExecutor);

        CompletableFuture.runAsync(taskManager)
                .whenComplete((msg, ex) -> log.info("Configuration executed " + LocalDateTime.now()));
    }

    private void setVerifyTouchpointAnyIfNull(BundleRequest bundleRequest) {
        String touchpoint = bundleRequest.getTouchpoint();

        if (touchpoint == null) {
            bundleRequest.setTouchpoint("ANY");
        } else {
            if (touchpointRepository.findByName(touchpoint).isEmpty()) {
                throw new AppException(AppError.TOUCHPOINT_NOT_FOUND, touchpoint);
            }
        }
    }

    /**
     * find CI-Bundle by fiscalCode and idBundle if exists from DB
     *
     * @param fiscalCode fiscal code of the CI
     * @param idBundle   id of the bundle
     * @return the CI-Bundle relation if exists, otherwise throws an exception
     * @throws AppException if the CI-Bundle relation does not exist
     */
    private CiBundle findCiBundle(@NotNull String fiscalCode, @NotNull String idBundle) {
        return ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(idBundle, fiscalCode)
                .orElseThrow(() -> new AppException(AppError.CI_BUNDLE_NOT_FOUND, idBundle, fiscalCode));
    }

    /**
     * Check ciBundle consistency
     *
     * @param ciBundle CI Bundle
     * @param idPSP    PSP identifier
     * @return check if the bundle referenced in ciBundle exists and is linked to the same PSP
     */
    private boolean checkCiBundle(CiBundle ciBundle, String idPSP) {
        return bundleRepository.findById(ciBundle.getIdBundle(), new PartitionKey(idPSP)).isPresent();
    }

    /**
     * Retrieve a bundle by id
     *
     * @param idBundle Bundle identifier
     * @return bundle
     */
    private Bundle getBundle(String idBundle) {
        return this.bundleRepository.findById(idBundle)
                .orElseThrow(() -> new AppException(AppError.BUNDLE_NOT_FOUND, idBundle));
    }

    /**
     * Retrieve a bundle by id and partition key
     *
     * @param idBundle Bundle identifier
     * @param idPsp    PSP identifier
     * @return bundle
     */
    private Bundle getBundle(String idBundle, String idPsp) {
        Optional<Bundle> bundle = bundleRepository.findById(idBundle, new PartitionKey(idPsp));
        if (bundle.isEmpty()) {
            throw new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        }

        return bundle.get();
    }

    /**
     * Verify if payment amount range overlaps the target one
     *
     * @param minPaymentAmount       min amount of bundle request
     * @param maxPaymentAmount       max amount of bundle request
     * @param minPaymentAmountTarget min amount of existent bundle
     * @param maxPaymentAmountTarget max amount of existent bundle
     */
    private boolean isPaymentAmountRangeValid(
            Long minPaymentAmount,
            Long maxPaymentAmount,
            Long minPaymentAmountTarget,
            Long maxPaymentAmountTarget
    ) {
        return minPaymentAmount < maxPaymentAmount && (
                minPaymentAmount < minPaymentAmountTarget && maxPaymentAmount < minPaymentAmountTarget || minPaymentAmount >= maxPaymentAmountTarget
        );
    }

    /**
     * Verify if date is equal or after now
     *
     * @param date  date to check
     * @param equal true if check equal
     * @return true if {@code date} is after now
     */
    private boolean isDateAcceptable(LocalDate date, boolean equal) {
        LocalDate now = LocalDate.now();
        return equal ? date.isEqual(now) || date.isAfter(now) : date.isAfter(now);
    }

    /**
     * Verify if validDateFrom is acceptable according to target validityDateTo
     */
    private boolean isValidityDateFromValid(LocalDate validityDateFrom, LocalDate validityDateToTarget) {
        return validityDateToTarget != null && !validityDateFrom.isBefore(validityDateToTarget) && !validityDateFrom.isEqual(validityDateToTarget);
    }

    /**
     * Verify if validDateTo of the request is before or after the validityDateTo of the existent bundle
     */
    private boolean isValidityDateToLater(LocalDate validityDateToBundleRequest, LocalDate validityDateToBundleExistent) {
        return validityDateToBundleRequest.isAfter(validityDateToBundleExistent);
    }

    /**
     * If date is null, returns the next acceptable date
     *
     * @return date
     */
    private LocalDate getNextAcceptableDate(LocalDate date) {
        // verify date: if null set to now +1
        if (date == null) {
            date = LocalDate.now().plusDays(1);
        }
        return date;
    }

    /**
     * Verify if bundleRequest has got acceptable validityDateFrom and validityDateFrom
     */
    private void analyzeValidityDate(BundleRequest bundleRequest, Bundle bundle) {
        // if it is a create operation (bundle == null) or an update and the ValidityDateFrom has been changed: its correctness is checked
        if ((null == bundle || !bundleRequest.getValidityDateFrom().equals(bundle.getValidityDateFrom())) && (!isDateAcceptable(bundleRequest.getValidityDateFrom(), true))) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDateFrom should be set equal or after now.");

        }

        // if it is an update operation: check if validityDateTo is not expired (is after now)
        if (null != bundle && bundle.getValidityDateTo() != null && LocalDate.now().isAfter(bundle.getValidityDateTo())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }


        if (bundleRequest.getValidityDateTo() != null) {
            // verify if validityDateTo is after now
            if (!isDateAcceptable(bundleRequest.getValidityDateTo(), false)) {
                throw new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDateTo should be set after now.");
            }

            // check it is before validityDateTo
            if (bundleRequest.getValidityDateTo().isBefore(bundleRequest.getValidityDateFrom())) {
                throw new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDateTo is before of ValidityDateFrom.");
            }
        }
    }

    /**
     * Verify the amount range set in bundle request is correct
     */
    private void analyzeMinMaxPaymentAmount(BundleRequest bundleRequest) {
        if (bundleRequest.getMinPaymentAmount() >= bundleRequest.getMaxPaymentAmount()) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, "Amount range not valid. MaxPaymentAmount must be >= than MinPaymentAmount");
        }
    }


    /**
     * Verify if paymentType exists in the related container
     */
    private PaymentType getPaymentTypeByName(String paymentTypeName) {
        return paymentTypeRepository.findByName(paymentTypeName)
                .orElseThrow(() -> new AppException(AppError.PAYMENT_TYPE_NOT_FOUND, paymentTypeName));
    }

    /**
     * Verify if the request could be accepted according to the existent bundles
     */
    private void analyzeBundlesOverlapping(String idBundle, String idPsp, BundleRequest bundleRequest) {
        // check if exists already the same configuration (minPaymentAmount, maxPaymentAmount, paymentType, touchpoint, type, transferCategoryList)
        // if it exists check validityDateFrom of the new configuration is next to validityDateTo of the existing one
        // check if the same payment amount range must not have the same tuple (paymentType, touchpoint, type, transferCategoryList)
        // check if there is overlapping transferCategoryList

        List<Bundle> bundles = getBundlesIdPspTypePaymentTypeTouchPoint(idPsp, bundleRequest);

        bundles.forEach(bundle -> {
            // If bundles have same id SKIP configuration check (UPDATE operation)
            // If bundles have different onUs flag SKIP configuration check
            // If bundles are private AND have different channels SKIP configuration check
            // ELSE CHECK if they have the same configuration
            if (bundle.getId().equals(idBundle)) {
                return;
            }
            if (bundle.getOnUs() != null && !bundle.getOnUs().equals(bundleRequest.getOnUs())) {
                return;
            }
            if (bundleRequest.getType().equals(BundleType.PRIVATE) && (!bundleRequest.getIdChannel().equals(bundle.getIdChannel()))) {
                return;
            }
            if (
                // verify payment amount range validity
                    !isPaymentAmountRangeValid(bundleRequest.getMinPaymentAmount(), bundleRequest.getMaxPaymentAmount(), bundle.getMinPaymentAmount(), bundle.getMaxPaymentAmount()) &&
                            // verify transfer category list overlapping and verify
                            !isTransferCategoryListValid(bundleRequest.getTransferCategoryList(), bundle.getTransferCategoryList()) &&
                            // verify if validityDateFrom is acceptable
                            !isValidityDateFromValid(bundleRequest.getValidityDateFrom(), bundle.getValidityDateTo()) &&
                            // verify if validityDateTo of request is after the validityDateTo of the existing bundle (only for private bundles)
                            !(bundleRequest.getType().equals(BundleType.PRIVATE) && isValidityDateToLater(bundleRequest.getValidityDateTo(), bundle.getValidityDateTo()))
            ) {
                throw new AppException(AppError.BUNDLE_BAD_REQUEST, "Bundle configuration overlaps an existing one.");
            }
        });
    }

    /**
     * Verify if transferCategoryList overlaps the target one.
     */
    private boolean isTransferCategoryListValid(
            List<String> transferCategoryList,
            List<String> transferCategoryListTarget
    ) {
        // If transfer & transferTarget are NOT both null
        return transferCategoryList != transferCategoryListTarget &&
                // If the transfer list does not match
                ((transferCategoryList == null || transferCategoryListTarget == null) ||
                transferCategoryList.stream().noneMatch(transferCategoryListTarget::contains));
    }

    private List<Bundle> getBundlesByNameAndType(
            String idPsp,
            String name,
            List<BundleType> types,
            Sort.Direction maxPaymentAmountOrder,
            Long paymentAmountMinRange,
            Long paymentAmountMaxRange,
            LocalDate validBefore,
            LocalDate validAfter,
            LocalDate expireBefore,
            LocalDate expireAfter,
            Integer pageSize,
            Integer pageNumber
    ) {
        return cosmosRepository.getBundlesByNameAndType(idPsp, name, types, maxPaymentAmountOrder, paymentAmountMinRange, paymentAmountMaxRange, validBefore, validAfter, expireBefore, expireAfter, pageNumber, pageSize);
    }

    private List<Bundle> getBundlesIdPspTypePaymentTypeTouchPoint(String idPsp, BundleRequest bundleRequest) {
        if (Optional.ofNullable(bundleRequest.getPaymentType()).isPresent()) {
            return bundleRepository.findByIdPspAndTypeAndPaymentTypeAndTouchpoint(idPsp, bundleRequest.getType(), getPaymentTypeByName(bundleRequest.getPaymentType()).getName(), bundleRequest.getTouchpoint());
        } else {
            return bundleRepository.findByIdPspAndTypeAndTouchpointAndPaymentTypeIsNull(idPsp, bundleRequest.getType(), bundleRequest.getTouchpoint());
        }
    }
}
