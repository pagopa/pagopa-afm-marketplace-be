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
    private final BundleRepository bundleRepository;
    private final CiBundleRepository ciBundleRepository;
    private final ValidBundleRepository validBundleRepository;

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
            ValidBundle validBundle = new ValidBundle();
            validBundle.setId(bundle.getId());
            validBundle.setIdCdi(bundle.getIdCdi());
            validBundle.setIdPsp(bundle.getIdPsp());
            validBundle.setAbi(bundle.getAbi());
            validBundle.setIdBrokerPsp(bundle.getIdBrokerPsp());
            validBundle.setIdChannel(bundle.getIdChannel());
            validBundle.setName(bundle.getName());
            validBundle.setDescription(bundle.getDescription());
            validBundle.setPaymentAmount(bundle.getPaymentAmount());
            validBundle.setMinPaymentAmount(bundle.getMinPaymentAmount());
            validBundle.setMaxPaymentAmount(bundle.getMaxPaymentAmount());
            validBundle.setPaymentType(bundle.getPaymentType());
            validBundle.setTouchpoint(bundle.getTouchpoint());
            validBundle.setType(bundle.getType());
            validBundle.setTransferCategoryList(bundle.getTransferCategoryList());
            validBundle.setCiBundleList(ciBundleList);
            validBundle.setValidityDateTo(bundle.getValidityDateTo());
            validBundle.setValidityDateFrom(bundle.getValidityDateFrom());
            return validBundle;
        }).collect(Collectors.toList());

        validBundleRepository.deleteAll();
        validBundleRepository.saveAll(validBundleList);

        log.debug("Valid bundles created");
    }

}
