package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundleOffer;
import it.pagopa.afm.marketplacebe.entity.BundleOffer;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class ArchiveBundleOfferTask implements Runnable {

    private BundleOfferRepository bundleOfferRepository;

    private ArchivedBundleOfferRepository archivedBundleOfferRepository;

    private ModelMapper modelMapper;

    private LocalDate now;

    public ArchiveBundleOfferTask(
            BundleOfferRepository bundleOfferRepository,
            ArchivedBundleOfferRepository archivedBundleOfferRepository,
            LocalDate now) {
        this.bundleOfferRepository = bundleOfferRepository;
        this.archivedBundleOfferRepository = archivedBundleOfferRepository;
        this.now = now;
        this.modelMapper = new ModelMapper();
    }

    @Override
    public void run() {
        List<BundleOffer> offers = bundleOfferRepository.findByValidityDateToBefore(now);

        List<ArchivedBundleOffer> archivedBundles = offers.parallelStream().map(b -> modelMapper.map(b, ArchivedBundleOffer.class)).collect(Collectors.toList());
        archivedBundleOfferRepository.saveAll(archivedBundles);

        bundleOfferRepository.deleteAll(offers);

    }
}
