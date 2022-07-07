package it.pagopa.afm.marketplacebe.service;

import com.azure.cosmos.models.PartitionKey;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.entity.*;
import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.BundleRequest;
import it.pagopa.afm.marketplacebe.model.bundle.*;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
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
import java.util.stream.Collectors;

@Service
public class BundleService {

    @Autowired
    private BundleRepository bundleRepository;

    @Autowired
    private CiBundleRepository ciBundleRepository;

    @Autowired
    private ModelMapper modelMapper;

    // TODO: add pagination
    // TODO: add filter
    public Bundles getBundlesByIdPsp(String idPsp, Integer pageNumber, Integer limit) {
        List<BundleDetails> bundleList = bundleRepository
                .findByIdPsp(idPsp)
                .stream()
                .map(bundle -> modelMapper.map(bundle, BundleDetails.class))
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

    public BundleDetails getBundleById(String idBundle, String idPsp) {
        Bundle bundle = getBundle(idBundle, idPsp);

        return modelMapper.map(bundle, BundleDetails.class);
    }

    public BundleResponse createBundle(String idPsp, BundleRequest bundleRequest) {

        LocalDate validityDateFrom = bundleRequest.getValidityDateFrom() != null ? bundleRequest.getValidityDateFrom().toLocalDate() : LocalDate.now();
        LocalDate validityDateTo = bundleRequest.getValidityDateTo() != null ? bundleRequest.getValidityDateTo().toLocalDate() : null;
        if (bundleRequest.getValidityDateTo() != null && bundleRequest.getValidityDateTo().isBefore(validityDateFrom.atStartOfDay())) {
            throw new AppException(AppError.BUNDLE_BAD_REQUEST, "ValidityDateTo is null or before ValidityDateFrom");
        }

        if(bundleRepository.findByName(bundleRequest.getName(), new PartitionKey(idPsp)).isPresent()){
            throw new AppException(AppError.BUNDLE_NAME_CONFLICT, bundleRequest.getName());
        }

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
                .validityDateFrom(validityDateFrom)
                .validityDateTo(validityDateTo)
                .insertedDate(now)
                .lastUpdatedDate(now)
                .build();
        bundleRepository.save(bundle);
        return BundleResponse.builder()
                .idBundle(bundle.getId())
                .build();
    }

    public Bundle updateBundle(String idPsp, String idBundle, BundleRequest bundleRequest) {
        Bundle bundle = getBundle(idBundle, idPsp);
        Optional<Bundle> duplicateBundle = bundleRepository.findByName(bundleRequest.getName(), new PartitionKey(idPsp));


        if(duplicateBundle.isPresent() && !duplicateBundle.get().getId().equals(idBundle)){
            throw new AppException(AppError.BUNDLE_NAME_CONFLICT, bundleRequest.getName());
        }

        bundle.setName(bundleRequest.getName());
        bundle.setDescription(bundleRequest.getDescription());
        bundle.setPaymentAmount(bundleRequest.getPaymentAmount());
        bundle.setMinPaymentAmount(bundleRequest.getMinPaymentAmount());
        bundle.setMaxPaymentAmount(bundleRequest.getMaxPaymentAmount());
        bundle.setPaymentAmount(bundleRequest.getPaymentAmount());
        bundle.setTouchpoint(Touchpoint.valueOf(bundleRequest.getTouchpoint()));
        bundle.setType(BundleType.valueOf(bundleRequest.getType()));
        bundle.setTransferCategoryList(bundleRequest.getTransferCategoryList());
        bundle.setValidityDateFrom(bundleRequest.getValidityDateFrom().toLocalDate());
        bundle.setValidityDateTo(bundleRequest.getValidityDateTo().toLocalDate());
        bundle.setLastUpdatedDate(LocalDateTime.now());

        return bundleRepository.save(bundle);
    }

    public void removeBundle(String idPsp, String idBundle) {
        Bundle bundle = getBundle(idBundle, idPsp);
        // TODO: Delete from main collection and store into archive
        // bundleRepository.delete(bundle);

        bundle.setValidityDateTo(LocalDate.now());
        bundleRepository.save(bundle);
    }

    public CiFiscalCodeList getCIs(String idBundle, String idPSP){
        List<CiBundle> subscriptions =  ciBundleRepository.findByIdBundle(idBundle);
        List<String> CIs = new ArrayList<>();
        CiFiscalCodeList ciFiscalCodeList = new CiFiscalCodeList();

        for(CiBundle ciBundle: subscriptions){
            if (!checkCiBundle(ciBundle, idPSP)){
                throw new AppException(AppError.BUNDLE_PSP_CONFLICT, idBundle, idPSP);
            }
            CIs.add(ciBundle.getCiFiscalCode());
        }
        ciFiscalCodeList.setCiFiscalCodeList(CIs);

        return ciFiscalCodeList;
    }

    public CiBundleDetails getCIDetails(String idBundle, String idPsp, String ciFiscalCode){
        Bundle bundle = getBundle(idBundle, idPsp);

        Optional<CiBundle> ciBundle = ciBundleRepository.findByIdBundleAndCiFiscalCodeAndValidityDateToIsNull(bundle.getId(), ciFiscalCode);

        if(ciBundle.isEmpty()){
            throw new AppException(AppError.CI_BUNDLE_NOT_FOUND, idBundle, ciFiscalCode);
        }

        return CiBundleDetails.builder()
                .validityDateTo(ciBundle.get().getValidityDateTo().toLocalDate())
                .attributes(
                        ciBundle.get().getAttributes() == null || ciBundle.get().getAttributes().isEmpty()
                                ? new ArrayList<>() : ciBundle.get().getAttributes().stream().map(
                                attribute -> modelMapper.map(
                                        attribute, it.pagopa.afm.marketplacebe.model.bundle.CiBundleAttribute.class)
                        ).collect(Collectors.toList()))
                .build();
    }

    public Bundles getBundlesByFiscalCode(@NotNull String fiscalCode, Integer limit, Integer pageNumber) {
        var bundleList = ciBundleRepository
                .findByCiFiscalCode(fiscalCode)
                .parallelStream()
                .map(ciBundle -> bundleRepository.findById(ciBundle.getIdBundle()))
                .map(bundle -> modelMapper.map(bundle, BundleDetails.class))
                .collect(Collectors.toList());

        return Bundles.builder()
                .bundleDetailsList(bundleList)
                .build();
    }

    public BundleDetails getBundleByFiscalCode(@NotNull String fiscalCode, @NotNull String idBundle) {
        var ciBundle = findCiBundle(fiscalCode, idBundle);

        var bundle = bundleRepository.findById(ciBundle.getIdBundle())
                .orElseThrow(() -> new AppException(AppError.BUNDLE_NOT_FOUND, idBundle));

        return modelMapper.map(bundle, BundleDetails.class);
    }

    public BundleDetailsAttributes getBundleAttributesByFiscalCode(@NotNull String fiscalCode, @NotNull String idBundle) {
        var ciBundle = findCiBundle(fiscalCode, idBundle);

        return modelMapper.map(ciBundle, BundleDetailsAttributes.class);
    }

    public BundleAttributeResponse createBundleAttributesByCi(@NotNull String fiscalCode, @NotNull String idBundle, @NotNull CiBundleAttributeModel bundleAttribute) {
        // bundle attribute should be created only for global bundle
        // for public bundle CI should send a new request to PSP
        Bundle bundle = getBundle(idBundle);

        if (!bundle.getType().equals(BundleType.GLOBAL)) {
            throw new AppException(AppError.CI_BUNDLE_BAD_REQUEST, String.format("Bundle with id %s is not global.", idBundle));
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

        if (!bundle.getType().equals(BundleType.GLOBAL)) {
            throw new AppException(AppError.CI_BUNDLE_BAD_REQUEST, String.format("Bundle with id %s is not global.", idBundle));
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
        }
        else {
            ciBundleRepository.save(ciBundle);
            // if bundle is global and there are no attributes -> remove ci-bundle relationship
            // in order to maintain logical relationship instead of physical one
            if (bundle.getType().equals(BundleType.GLOBAL) && ciBundle.getAttributes().size() == 0) {
                ciBundleRepository.delete(ciBundle);
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

    /** Check ciBundle consistency
     *
     * @param ciBundle CI Bundle
     * @param idPSP PSP identifier
     * @return check if the bundle referenced in ciBundle exists and is linked to the same PSP
     */
    private boolean checkCiBundle(CiBundle ciBundle, String idPSP){
        return bundleRepository.findById(ciBundle.getIdBundle(), new PartitionKey(idPSP)).isPresent();
    }

    /** Retrieve a bundle by id
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

    /** Retrieve a bundle by id and partition key
     *
     * @param idBundle Bundle identifier
     * @param idPsp PSP identifier
     * @return bundle
     */
    private Bundle getBundle(String idBundle, String idPsp) {
        Optional<Bundle> bundle = bundleRepository.findById(idBundle, new PartitionKey(idPsp));
        if (bundle.isEmpty()) {
            throw new AppException(AppError.BUNDLE_NOT_FOUND, idBundle);
        }

        return bundle.get();
    }

}
