package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.service.CalculatorService;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDateTime;

@Slf4j
public class CalculatorTask implements Runnable {

    private CalculatorService calculatorService;
    private BundleRepository bundleRepository;
    private CiBundleRepository ciBundleRepository;

    public CalculatorTask(CalculatorService calculatorService, BundleRepository bundleRepository, CiBundleRepository ciBundleRepository) {
        this.bundleRepository = bundleRepository;
        this.calculatorService = calculatorService;
        this.ciBundleRepository = ciBundleRepository;
    }

    @Override
    public void run() {
        log.warn("Calling calculator " + LocalDateTime.now() + " | " + Thread.currentThread().getName());
        calculatorService.configure();
    }
}
