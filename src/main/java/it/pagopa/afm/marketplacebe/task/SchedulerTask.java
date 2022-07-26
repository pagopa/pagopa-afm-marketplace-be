package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.service.CalculatorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

@Component
public class SchedulerTask {

    @Autowired
    private ThreadPoolTaskScheduler taskScheduler;

    @Autowired
    private BundleRepository bundleRepository;

    @Autowired
    private BundleOfferRepository bundleOfferRepository;

    @Autowired
    private BundleRequestRepository bundleRequestRepository;

    @Autowired
    private CiBundleRepository ciBundleRepository;

    @Autowired
    private ArchivedBundleRepository archivedBundleRepository;

    @Autowired
    private ArchivedBundleOfferRepository archivedBundleOfferRepository;

    @Autowired
    private ArchivedBundleRequestRepository archivedBundleRequestRepository;

    @Autowired
    private ArchivedCiBundleRepository archivedCiBundleRepository;

    @Autowired
    private CalculatorService calculatorService;

    @Autowired
    private CronTrigger cronTrigger;

    @PostConstruct
    public void scheduleRunnableWithCronTrigger() {

        BundleTaskExecutor bundleArchiver = new BundleTaskExecutor(bundleRepository, archivedBundleRepository);
        BundleOfferTaskExecutor bundleOfferArchiver = new BundleOfferTaskExecutor(bundleOfferRepository, archivedBundleOfferRepository);
        BundleRequestTaskExecutor bundleRequestArchiver = new BundleRequestTaskExecutor(bundleRequestRepository, archivedBundleRequestRepository);
        CiBundleTaskExecutor ciBundleArchiver = new CiBundleTaskExecutor(ciBundleRepository, archivedCiBundleRepository);
        CalculatorTaskExecutor calculatorTaskExecutor = new CalculatorTaskExecutor(calculatorService, bundleRepository, ciBundleRepository);

        taskScheduler.schedule(new TaskManager(
                bundleArchiver,
                bundleOfferArchiver,
                bundleRequestArchiver,
                ciBundleArchiver,
                        calculatorTaskExecutor,
                taskScheduler.getScheduledThreadPoolExecutor()),
                cronTrigger);
    }
}
