package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundle;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class ArchiveBundleTask implements Runnable {

    private BundleRepository bundleRepository;

    private ArchivedBundleRepository archivedBundleRepository;

    private ModelMapper modelMapper;

    private LocalDate now;

    public ArchiveBundleTask(
            BundleRepository bundleRepository,
            ArchivedBundleRepository archivedBundleRepository,
            LocalDate now) {
        this.bundleRepository = bundleRepository;
        this.archivedBundleRepository = archivedBundleRepository;
        this.now = now;
        this.modelMapper = new ModelMapper();
    }

    @Override
    public void run() {
        List<Bundle> bundles = bundleRepository.findByValidityDateToBefore(now);

        List<ArchivedBundle> archivedBundles = bundles.parallelStream().map(b -> modelMapper.map(b, ArchivedBundle.class)).collect(Collectors.toList());
        archivedBundleRepository.saveAll(archivedBundles);

        bundleRepository.deleteAll(bundles);

    }
}
