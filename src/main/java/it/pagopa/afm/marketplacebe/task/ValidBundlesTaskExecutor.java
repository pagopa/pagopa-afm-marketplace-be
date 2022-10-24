package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.entity.ValidBundle;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.ValidBundleRepository;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class ValidBundlesTaskExecutor extends TaskExecutor {
    private BundleRepository bundleRepository;
    private CiBundleRepository ciBundleRepository;
    private ValidBundleRepository validBundleRepository;

    public ValidBundlesTaskExecutor(BundleRepository bundleRepository,
                                    CiBundleRepository ciBundleRepository,
                                    ValidBundleRepository validBundleRepository
    ) {
        this.bundleRepository = bundleRepository;
        this.ciBundleRepository = ciBundleRepository;
        this.validBundleRepository = validBundleRepository;
    }

    @Override
    public void execute() {
        generateConfiguration();
    }

    private void generateConfiguration() {
        List<Bundle> bundles = new ArrayList<>();
        bundleRepository.findAll().forEach(bundles::add);

        List<CiBundle> ciBundles = new ArrayList<>();
        ciBundleRepository.findAll().forEach(ciBundles::add);

        List<ValidBundle> validBundleList = bundles.parallelStream().map(bundle -> {
            List<CiBundle> ciBundleList = ciBundles.stream().filter(ciBundle -> ciBundle.getIdBundle().equals(bundle.getId())).collect(Collectors.toList());
            return ValidBundle.builder()
                    .id(bundle.getId())
                    .idPsp(bundle.getIdPsp())
                    .name(bundle.getName())
                    .description(bundle.getDescription())
                    .paymentAmount(bundle.getPaymentAmount())
                    .minPaymentAmount(bundle.getMinPaymentAmount())
                    .maxPaymentAmount(bundle.getMaxPaymentAmount())
                    .paymentMethod(bundle.getPaymentMethod())
                    .touchpoint(bundle.getTouchpoint())
                    .type(bundle.getType())
                    .transferCategoryList(bundle.getTransferCategoryList())
                    .ciBundleList(ciBundleList)
                    .validityDateTo(bundle.getValidityDateTo())
                    .validityDateFrom(bundle.getValidityDateFrom())
                    .build();
        }).collect(Collectors.toList());

        validBundleRepository.deleteAll();
        validBundleRepository.saveAll(validBundleList);

        log.debug("Valid bundles created");
    }

}
