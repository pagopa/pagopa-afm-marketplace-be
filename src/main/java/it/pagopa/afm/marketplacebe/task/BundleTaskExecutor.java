package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundle;
import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class BundleTaskExecutor extends TaskExecutor {

    private BundleRepository bundleRepository;

    private ArchivedBundleRepository archivedBundleRepository;

    public BundleTaskExecutor(
            BundleRepository bundleRepository,
            ArchivedBundleRepository archivedBundleRepository) {
        super();

        this.bundleRepository = bundleRepository;
        this.archivedBundleRepository = archivedBundleRepository;
    }

    @Override
    public void execute() {
        List<Bundle> bundles = bundleRepository.findByValidityDateToBefore(now);

        List<ArchivedBundle> archivedBundles = bundles.parallelStream().map(b -> modelMapper.map(b, ArchivedBundle.class)).collect(Collectors.toList());
        archivedBundleRepository.saveAll(archivedBundles);

        bundleRepository.deleteAll(bundles);
        log.debug("Bundle archived");
    }
}
