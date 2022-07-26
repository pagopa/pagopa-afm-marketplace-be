package it.pagopa.afm.marketplacebe.task;


import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.time.LocalDate;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

class ArchiveBundleTaskTest {

    @MockBean
    BundleRepository bundleRepository;

    @MockBean
    BundleOfferRepository bundleOfferRepository;

    @MockBean
    BundleRequestRepository bundleRequestRepository;

    @MockBean
    CiBundleRepository ciBundleRepository;

    @MockBean
    ArchivedBundleRepository archivedBundleRepository;

    @MockBean
    ArchivedBundleOfferRepository archivedBundleOfferRepository;

    @MockBean
    ArchivedBundleRequestRepository archivedBundleRequestRepository;

    @MockBean
    ArchivedCiBundleRepository archivedCiBundleRepository;

    @Test
    public void archiveBundleTask() {
        LocalDate now = LocalDate.now();
        ArchiveBundleTask task = new ArchiveBundleTask(bundleRepository, archivedBundleRepository, now);

        ExecutorService executor = Executors.newCachedThreadPool();
        executor.execute(task);
    }

    @Test
    public void archiveBundleOfferTask() {
        LocalDate now = LocalDate.now();
        ArchiveBundleOfferTask task = new ArchiveBundleOfferTask(bundleOfferRepository, archivedBundleOfferRepository, now);

        ExecutorService executor = Executors.newCachedThreadPool();
        executor.execute(task);
    }

    @Test
    public void archiveBundleRequestTask() {
        LocalDate now = LocalDate.now();
        ArchiveBundleRequestTask task = new ArchiveBundleRequestTask(bundleRequestRepository, archivedBundleRequestRepository, now);

        ExecutorService executor = Executors.newCachedThreadPool();
        executor.execute(task);
    }

    @Test
    public void archiveCiBundleTask() {
        LocalDate now = LocalDate.now();
        ArchiveCiBundleTask task = new ArchiveCiBundleTask(ciBundleRepository, archivedCiBundleRepository, now);

        ExecutorService executor = Executors.newCachedThreadPool();
        executor.execute(task);
    }

}
