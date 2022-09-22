package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.model.CalculatorConfiguration;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.service.CalculatorService;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@Slf4j
public class CalculatorTaskExecutor extends TaskExecutor {

    private CalculatorService calculatorService;
    private BundleRepository bundleRepository;
    private CiBundleRepository ciBundleRepository;

    public CalculatorTaskExecutor(CalculatorService calculatorService, BundleRepository bundleRepository, CiBundleRepository ciBundleRepository) {
        this.bundleRepository = bundleRepository;
        this.calculatorService = calculatorService;
        this.ciBundleRepository = ciBundleRepository;
    }

    @Override
    public void execute() {
//        calculatorService.configure(getConfiguration());
    }

    public CalculatorConfiguration getConfiguration() {
        List<Bundle> bundles = new ArrayList<>();
        bundleRepository.findAll().forEach(bundles::add);

        List<CiBundle> ciBundles = new ArrayList<>();
        ciBundleRepository.findAll().forEach(ciBundles::add);

        return CalculatorConfiguration.builder()
                .bundles(bundles)
                .ciBundles(ciBundles)
                .build();
    }
}
