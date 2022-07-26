package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.service.CalculatorService;
import lombok.extern.slf4j.Slf4j;

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
        calculatorService.configure();
    }
}
