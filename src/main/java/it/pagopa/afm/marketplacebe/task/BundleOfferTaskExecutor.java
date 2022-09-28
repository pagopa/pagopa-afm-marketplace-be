package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class BundleOfferTaskExecutor extends TaskExecutor {

    private final BundleOfferRepository bundleOfferRepository;

    private final ArchivedBundleOfferRepository archivedBundleOfferRepository;

    public BundleOfferTaskExecutor(
            BundleOfferRepository bundleOfferRepository,
            ArchivedBundleOfferRepository archivedBundleOfferRepository) {
        super();

        this.bundleOfferRepository = bundleOfferRepository;
        this.archivedBundleOfferRepository = archivedBundleOfferRepository;
    }

    @Override
    public void execute() {
        List<BundleOffer> offers = bundleOfferRepository.findByValidityDateToBefore(now);

        List<ArchivedBundleOffer> archivedBundles = offers.parallelStream().map(b -> modelMapper.map(b, ArchivedBundleOffer.class)).collect(Collectors.toList());
        archivedBundleOfferRepository.saveAll(archivedBundles);

        bundleOfferRepository.deleteAll(offers);
        log.debug("Bundle Offer archived");
    }
}
