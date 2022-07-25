package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.time.LocalDate;
import java.util.Date;

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
    private CronTrigger cronTrigger;

    @PostConstruct
    public void scheduleRunnableWithCronTrigger() {

        taskScheduler.schedule(new TaskManager(
                bundleRepository,
                bundleOfferRepository,
                bundleRequestRepository,
                ciBundleRepository,
                archivedBundleRepository,
                archivedBundleOfferRepository,
                archivedBundleRequestRepository,
                archivedCiBundleRepository,
                LocalDate.now(),
                taskScheduler.getScheduledThreadPoolExecutor()), cronTrigger);
    }
}
