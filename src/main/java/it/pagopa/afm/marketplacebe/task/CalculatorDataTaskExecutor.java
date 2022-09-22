package it.pagopa.afm.marketplacebe.task;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.model.CalculatorConfiguration;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.service.CalculatorService;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@Slf4j
public class CalculatorDataTaskExecutor extends TaskExecutor {
    private String volume;

    private CalculatorService calculatorService;
    private BundleRepository bundleRepository;
    private CiBundleRepository ciBundleRepository;

    public CalculatorDataTaskExecutor(CalculatorService calculatorService, BundleRepository bundleRepository, CiBundleRepository ciBundleRepository, String volume) {
        this.bundleRepository = bundleRepository;
        this.calculatorService = calculatorService;
        this.ciBundleRepository = ciBundleRepository;
        this.volume = volume;
    }

    @Override
    public void execute() {

        List<Bundle> bundles = new ArrayList<>();
        bundleRepository.findAll().forEach(bundles::add);

        List<CiBundle> ciBundles = new ArrayList<>();
        ciBundleRepository.findAll().forEach(ciBundles::add);

        CalculatorConfiguration configuration = CalculatorConfiguration.builder()
                .bundles(bundles)
                .ciBundles(ciBundles)
                .build();

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        LocalDate now = LocalDate.now();
        try {
            String filename = String.format("configuration_%s_%s_%s.json", now.getYear(), now.getMonthValue(), now.getDayOfMonth());
            objectMapper.writeValue(new File(String.format("%s/%s", volume, filename)), configuration);
            calculatorService.configure();
        } catch (IOException e) {
            log.error("Problem to save configuration: ", e);
            throw new RuntimeException(e);
        }
        log.debug("Calculator Data configured");
    }
}
