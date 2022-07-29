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

import java.io.FileWriter;
import java.io.IOException;
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
        List<Bundle> bundles = new ArrayList<>();
        bundleRepository.findAll().forEach(bundles::add);

        List<CiBundle> ciBundles = new ArrayList<>();
        ciBundleRepository.findAll().forEach(ciBundles::add);

        CalculatorConfiguration configuration = CalculatorConfiguration.builder()
                .bundles(bundles)
                .ciBundles(ciBundles)
                .build();

//        try {
//            ObjectMapper mapper = new ObjectMapper();
//            mapper.registerModule(new JavaTimeModule());
//            // Constructs a FileWriter given a file name, using the platform's default charset
//            FileWriter file = new FileWriter("/tmp/data.txt");
//            file.write(mapper.writeValueAsString(configuration));
//        } catch (IOException e) {
//            e.printStackTrace();
//        }

        calculatorService.configure(configuration);
    }
}
