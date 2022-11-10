package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.entity.PaymentMethod;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.BundleAttributeResponse;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetailsAttributes;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetailsForCi;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.bundle.BundleResponse;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleDetails;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleInfo;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundles;
import it.pagopa.afm.marketplacebe.model.bundle.PspBundleDetails;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.TouchpointRepository;
import it.pagopa.afm.marketplacebe.repository.ValidBundleRepository;
import it.pagopa.afm.marketplacebe.task.BundleOfferTaskExecutor;
import it.pagopa.afm.marketplacebe.task.BundleRequestTaskExecutor;
import it.pagopa.afm.marketplacebe.task.BundleTaskExecutor;
import it.pagopa.afm.marketplacebe.task.CiBundleTaskExecutor;
import it.pagopa.afm.marketplacebe.task.TaskManager;
import it.pagopa.afm.marketplacebe.task.ValidBundlesTaskExecutor;
import it.pagopa.afm.marketplacebe.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@Slf4j
public class BundleService {

    public static final String ALREADY_DELETED = "Bundle has been deleted.";

    @Autowired
    private BundleRepository bundleRepository;

    @Autowired
    private CiBundleRepository ciBundleRepository;

    @Autowired
    private BundleRequestRepository bundleRequestRepository;

    @Autowired
    private BundleOfferRepository bundleOfferRepository;

    @Autowired
    private TouchpointRepository touchpointRepository;

    @Autowired
    private ArchivedBundleRepository archivedBundleRepository;

    @Autowired
    private ArchivedBundleOfferRepository archivedBundleOfferRepository;

    @Autowired
    private ArchivedBundleRequestRepository archivedBundleRequestRepository;

    @Autowired
    private ArchivedCiBundleRepository archivedCiBundleRepository;

    @Autowired
    private ValidBundleRepository validBundleRepository;

    @Autowired
    private ModelMapper modelMapper;

    public Bundles getBundles(List<BundleType> bundleTypes) {
        List<PspBundleDetails> bundleList = getValidBundleByType(bundleTypes)
                .stream()
                .map(bundle -> modelMapper.map(bundle, PspBundleDetails.class))
                .collect(Collectors.toList());

        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(bundleList.size())
                .totalPages(1)
                .build();

        return Bundles.builder()
                .bundleDetailsList(bundleList)
                .pageInfo(pageInfo)
                .build();
    }

    public Bundles getBundlesByIdPsp(String idPsp, Integer pageNumber, Integer limit) {
        List<PspBundleDetails> bundleList = bundleRepository
                .findByIdPsp(idPsp, new PartitionKey(idPsp))
                .stream()
                .map(bundle -> modelMapper.map(bundle, PspBundleDetails.class))
                .collect(Collectors.toList());

        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(bundleList.size())
                .totalPages(1)
                .limit(limit)
                .page(pageNumber)
                .build();

        return Bundles.builder()
                .bundleDetailsList(bundleList)
                .pageInfo(pageInfo)
                .build();
    }

    public PspBundleDetails getBundleById(String idBundle, String idPsp) {
        Bundle bundle = getBundle(idBundle, idPsp);

        return modelMapper.map(bundle, PspBundleDetails.class);
    }

    public BundleResponse createBundle(String idPsp, BundleRequest bundleRequest) {
        setPaymentMethodAnyIfNull(bundleRequest);
        setVerifyTouchpointAnyIfNull(bundleRequest);

        // verify validityDateFrom, if null set to now +1d
        bundleRequest.setValidityDateFrom(getNextAcceptableDate(bundleRequest.getValidityDateFrom()));

        // verify validityDateFrom and validityDateTo
        analyzeValidityDate(bundleRequest);

        // check if exists already the same configuration (minPaymentAmount, maxPaymentAmount, paymentMethod, touchpoint, type, transferCategoryList)
        // if it exists check validityDateFrom of the new configuration is next to validityDateTo of the existing one
        // check if the same payment amount range must not have the same tuple (paymentMethod, touchpoint, type, transferCategoryList)
        // check if there is overlapping transferCategoryList
        analyzeBundlesOverlapping(idPsp, bundleRequest);

        // verify no bundle exists with the same name
        if (bundleRepository.findByNameAndIdPsp(bundleRequest.getName(), idPsp, new PartitionKey(idPsp)).isPresent()) {
            throw new AppException(AppError.BUNDLE_NAME_CONFLICT, bundleRequest.getName());
        }

        LocalDateTime now = LocalDateTime.now();
        Bundle bundle = Bundle.builder()
                .idPsp(idPsp)
                .idChannel(bundleRequest.getIdChannel())
                .idBrokerPsp(bundleRequest.getIdBrokerPsp())
                .name(bundleRequest.getName())
                .description(bundleRequest.getDescription())
                .paymentAmount(bundleRequest.getPaymentAmount())
                .minPaymentAmount(bundleRequest.getMinPaymentAmount())
                .maxPaymentAmount(bundleRequest.getMaxPaymentAmount())
                .paymentMethod(bundleRequest.getPaymentMethod())
                .onUs(bundleRequest.getPaymentMethod().equals(PaymentMethod.CP) && CommonUtil.deNull(bundleRequest.getOnUs()))
                .digitalStamp(CommonUtil.deNull(bundleRequest.getDigitalStamp()))
                .digitalStampRestriction(CommonUtil.deNull(bundleRequest.getDigitalStamp()) && CommonUtil.deNull(bundleRequest.getDigitalStampRestriction()))
                .touchpoint(bundleRequest.getTouchpoint())
                .type(bundleRequest.getType())
                .transferCategoryList(bundleRequest.getTransferCategoryList())
                .validityDateFrom(bundleRequest.getValidityDateFrom())
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
        setPaymentMethodAnyIfNull(bundleRequest);
        setVerifyTouchpointAnyIfNull(bundleRequest);

        Bundle bundle = getBundle(idBundle, idPsp);

        // check if validityDateTo is after now
        if (bundle.getValidityDateTo() != null && LocalDate.now().isAfter(bundle.getValidityDateTo())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        // verify validityDateFrom, if it is null set to now +1d
        bundleRequest.setValidityDateFrom(getNextAcceptableDate(bundleRequest.getValidityDateFrom()));

        // verify validityDateFrom and validityDateTo
        analyzeValidityDate(bundleRequest);

        // check if exists already the same configuration (minPaymentAmount, maxPaymentAmount, paymentMethod, touchpoint, type, transferCategoryList)
        // if it exists check validityDateFrom of the new configuration is next to validityDateTo of the existing one
        // check if the same payment amount range must not have the same tuple (paymentMethod, touchpoint, type, transferCategoryList)
        // check if there is overlapping transferCategoryList
        analyzeBundlesOverlapping(idPsp, bundleRequest);

        // verify the only other bundle with the same name is the bundle I want to modify
        Optional<Bundle> duplicateBundle = bundleRepository.findByNameAndIdNot(bundleRequest.getName(), idBundle, new PartitionKey(idPsp));
        if (duplicateBundle.isPresent()) {
            throw new AppException(AppError.BUNDLE_NAME_CONFLICT, bundleRequest.getName());
        }

        bundle.setIdChannel(bundleRequest.getIdChannel());
        bundle.setIdBrokerPsp(bundleRequest.getIdBrokerPsp());
        bundle.setName(bundleRequest.getName());
        bundle.setDescription(bundleRequest.getDescription());
        bundle.setPaymentAmount(bundleRequest.getPaymentAmount());
        bundle.setMinPaymentAmount(bundleRequest.getMinPaymentAmount());
        bundle.setMaxPaymentAmount(bundleRequest.getMaxPaymentAmount());
        bundle.setPaymentAmount(bundleRequest.getPaymentAmount());
        bundle.setTouchpoint(bundleRequest.getTouchpoint());
        bundle.setTransferCategoryList(bundleRequest.getTransferCategoryList());
        bundle.setValidityDateFrom(bundleRequest.getValidityDateFrom());
        bundle.setValidityDateTo(bundleRequest.getValidityDateTo());
        bundle.setLastUpdatedDate(LocalDateTime.now());
        bundle.setOnUs(bundleRequest.getPaymentMethod().equals(PaymentMethod.CP) && CommonUtil.deNull(bundleRequest.getOnUs()));
        bundle.setDigitalStamp(CommonUtil.deNull(bundleRequest.getDigitalStamp()));
        bundle.setDigitalStampRestriction(CommonUtil.deNull(bundleRequest.getDigitalStamp()) && CommonUtil.deNull(bundleRequest.getDigitalStampRestriction()));

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

    public void removeBundle(String idPsp, String idBundle) {
        Bundle bundle = getBundle(idBundle, idPsp);

        if (bundle.getValidityDateTo() != null) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, "Bundle has been already deleted.");
        }

        // set validityDateTo=now in order to invalidate the bundle (logical delete)
        bundle.setValidityDateTo(LocalDate.now());

        bundleRepository.save(bundle);

        // invalidate all references
        // ci-bundles
        List<CiBundle> ciBundleList = ciBundleRepository.findByIdBundle(idBundle);
        ciBundleList.forEach(ciBundle -> ciBundle.setValidityDateTo(LocalDate.now()));
        ciBundleRepository.saveAll(ciBundleList);

        // bundle requests
        List<BundleRequestEntity> requests = bundleRequestRepository.findByIdBundleAndIdPspAndAcceptedDateIsNullAndRejectionDateIsNull(idBundle, idPsp);

        requests.forEach(request -> request.setRejectionDate(LocalDateTime.now()));
        bundleRequestRepository.saveAll(requests);

        // bundle offers (if not accepted/rejected can be deleted physically)
        BundleOffer offer = bundleOfferRepository.findByIdPspAndIdBundleAndAcceptedDateIsNullAndRejectionDateIsNull(idPsp, idBundle);
        if (offer != null) {
            bundleOfferRepository.delete(offer);
        }
    }

    public CiFiscalCodeList getCIs(String idBundle, String idPSP) {
        List<CiBundle> subscriptions = ciBundleRepository.findByIdBundle(idBundle);
        List<String> ciList = new ArrayList<>();

        for (CiBundle ciBundle : subscriptions) {
            if (!checkCiBundle(ciBundle, idPSP)) {
                throw new AppException(AppError.BUNDLE_PSP_CONFLICT, idBundle, idPSP);
            }
            ciList.add(ciBundle.getCiFiscalCode());
        }
        return CiFiscalCodeList.builder()
                .ciFiscalCodeList(ciList)
                .build();
    }

    public CiBundleDetails getCIDetails(String idBundle, String idPsp, String ciFiscalCode) {
        Bundle bundle = getBundle(idBundle, idPsp);

        Optional<CiBundle> ciBundle = ciBundleRepository.findByIdBundleAndCiFiscalCode(bundle.getId(), ciFiscalCode);

        if (ciBundle.isEmpty()) {
            throw new AppException(AppError.CI_BUNDLE_NOT_FOUND, idBundle, ciFiscalCode);
        }

        // TODO use model mapper
        return CiBundleDetails.builder()
                .validityDateFrom(ciBundle.get().getValidityDateFrom())
                .validityDateTo(ciBundle.get().getValidityDateTo())
                .attributes(
                        ciBundle.get().getAttributes() == null || ciBundle.get().getAttributes().isEmpty()
                                ? new ArrayList<>() : ciBundle.get().getAttributes().stream().map(
                                attribute -> modelMapper.map(
                                        attribute, it.pagopa.afm.marketplacebe.model.bundle.CiBundleAttribute.class)
                        ).collect(Collectors.toList()))
                .build();
    }

    public CiBundles getBundlesByFiscalCode(@NotNull String fiscalCode, Integer limit, Integer pageNumber) {
        var bundleList = ciBundleRepository
                .findByCiFiscalCode(fiscalCode)
                .parallelStream()
                .map(ciBundle -> {
                    Bundle bundle = getBundle(ciBundle.getIdBundle());
                    CiBundleInfo ciBundleInfo = modelMapper.map(bundle, CiBundleInfo.class);
                    ciBundleInfo.setIdCiBundle(ciBundle.getId());
                    return ciBundleInfo;
                })
                .collect(Collectors.toList());

        return CiBundles.builder()
                .bundleDetailsList(bundleList)
                .pageInfo(PageInfo.builder()
                        .limit(limit)
                        .page(pageNumber)
                        .build())
                .build();
    }

    public BundleDetailsForCi getBundleByFiscalCode(@NotNull String fiscalCode, @NotNull String idBundle) {
        var ciBundle = findCiBundle(fiscalCode, idBundle);

        var bundle = getBundle(ciBundle.getIdBundle());

        return modelMapper.map(bundle, BundleDetailsForCi.class);
    }

    public void removeBundleByFiscalCode(@NotNull String fiscalCode, @NotNull String idCiBundle) {
        CiBundle ciBundle = ciBundleRepository.findById(idCiBundle, new PartitionKey(fiscalCode))
                .orElseThrow(() -> new AppException(AppError.CI_BUNDLE_ID_NOT_FOUND, idCiBundle));

        // set validityDateTo to now in order to be deleted by job at the next iteration
        ciBundle.setValidityDateTo(LocalDate.now());
        ciBundleRepository.save(ciBundle);
    }

    public BundleDetailsAttributes getBundleAttributesByFiscalCode(@NotNull String fiscalCode, @NotNull String idBundle) {
        var ciBundle = findCiBundle(fiscalCode, idBundle);

        return modelMapper.map(ciBundle, BundleDetailsAttributes.class);
    }

    public BundleAttributeResponse createBundleAttributesByCi(@NotNull String fiscalCode, @NotNull String idBundle, @NotNull CiBundleAttributeModel bundleAttribute) {
        // bundle attribute should be created only for global bundle
        // for public bundle CI should send a new request to PSP
        Bundle bundle = getBundle(idBundle);

        if (bundle.getValidityDateTo() != null) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, ALREADY_DELETED);
        }

        if (!bundle.getType().equals(BundleType.GLOBAL)) {
            throw new AppException(AppError.CI_BUNDLE_BAD_REQUEST, String.format("Bundle with id %s is not global.", idBundle));
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

        ciBundle.getAttributes().add(attribute);

        // save CI-Bundle with new attribute
        ciBundleRepository.save(ciBundle);

        return BundleAttributeResponse.builder()
                .idBundleAttribute(attribute.getId())
                .build();
    }

    public void updateBundleAttributesByCi(@NotNull String fiscalCode, @NotNull String idBundle, @NotNull String idAttribute, @NotNull CiBundleAttributeModel bundleAttribute) {
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

    public void removeBundleAttributesByCi(@NotNull String fiscalCode, @NotNull String idBundle, @NotNull String idAttribute) {
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

    private void setPaymentMethodAnyIfNull(BundleRequest bundleRequest) {
        if (bundleRequest.getPaymentMethod() == null) {
            bundleRequest.setPaymentMethod(PaymentMethod.ANY);
        }
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
        Optional<Bundle> bundle = bundleRepository.findById(idBundle);
        if (bundle.isEmpty()) {
            throw new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        }

        return bundle.get();
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
    private boolean isPaymentAmountRangeValid(Long minPaymentAmount, Long maxPaymentAmount, Long minPaymentAmountTarget, Long maxPaymentAmountTarget) {
        return minPaymentAmount < maxPaymentAmount && (
                minPaymentAmount < minPaymentAmountTarget && maxPaymentAmount < minPaymentAmountTarget || minPaymentAmount > maxPaymentAmountTarget
        );
    }

    /**
     * Verify if transferCategoryList overlaps the target one
     */
    private boolean isTransferCategoryListValid(List<String> transferCategoryList, List<String> transferCategoryListTarget) {
        return (transferCategoryListTarget == null) || (transferCategoryList != null && transferCategoryList.stream().noneMatch(transferCategoryListTarget::contains));
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
    private void analyzeValidityDate(BundleRequest bundleRequest) {
        // verify if validityDateFrom is equal or after now
        if (!isDateAcceptable(bundleRequest.getValidityDateFrom(), true)) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDateFrom should be set equal or after now.");
        }

        if (bundleRequest.getValidityDateTo() != null) {
            // verify if validityDateTo is after now
            if (!isDateAcceptable(bundleRequest.getValidityDateTo(), false)) {
                throw new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDateTo should be set after now.");
            }

            // check it is before validityDateTo
            if (bundleRequest.getValidityDateTo().isBefore(bundleRequest.getValidityDateFrom())) {
                throw new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDateTo is before of ValidityDateFrom");
            }
        }
    }

    /**
     * Verify if the request could be accepted according to the existent bundles
     */
    private void analyzeBundlesOverlapping(String idPsp, BundleRequest bundleRequest) {
        // check if exists already the same configuration (minPaymentAmount, maxPaymentAmount, paymentMethod, touchpoint, type, transferCategoryList)
        // if it exists check validityDateFrom of the new configuration is next to validityDateTo of the existing one
        // check if the same payment amount range must not have the same tuple (paymentMethod, touchpoint, type, transferCategoryList)
        // check if there is overlapping transferCategoryList

        List<Bundle> bundles = bundleRepository.findByIdPspAndTypeAndPaymentMethodAndTouchpoint(idPsp, bundleRequest.getType(), bundleRequest.getPaymentMethod(), bundleRequest.getTouchpoint());
        bundles.forEach(bundle -> {
            // verify payment amount range validity and
            // verify transfer category list overlapping and verify if validityDateFrom is acceptable
            if (!isPaymentAmountRangeValid(bundleRequest.getMinPaymentAmount(), bundleRequest.getMaxPaymentAmount(), bundle.getMinPaymentAmount(), bundle.getMaxPaymentAmount()) &&
                    !isTransferCategoryListValid(bundleRequest.getTransferCategoryList(), bundle.getTransferCategoryList()) &&
                    !isValidityDateFromValid(bundleRequest.getValidityDateFrom(), bundle.getValidityDateTo())) {
                throw new AppException(AppError.BUNDLE_BAD_REQUEST, "Bundle configuration overlaps an existing one.");
            }
        });
    }

    private List<Bundle> getValidBundleByType(List<BundleType> types) {
        switch (types.size()) {
            case 1:
                return bundleRepository.getValidBundleByType(types.get(0).getValue());
            case 2:
                return bundleRepository.getValidBundleByType(types.get(0).getValue(), types.get(1).getValue());
            case 3:
                return bundleRepository.getValidBundleByType(types.get(0).getValue(), types.get(1).getValue(),
                        types.get(2).getValue());
            default:
                throw new AppException(AppError.BUNDLE_BAD_REQUEST, "BundleType not specified");

        }
    }

}
