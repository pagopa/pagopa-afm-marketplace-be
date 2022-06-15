package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

@Service
public class BundleService {

    @Autowired
    private BundleRepository bundleRepository;

    // TODO: add pagination
    // TODO: add filter
    public Mono<Bundles> getBundlesByPage(Integer pageNumber, Integer limit, Bundle.Filter filter) {
        return bundleRepository.findAll().collectList().map(
                bundles -> new Bundles(bundles, new PageInfo())
        );
    }

    public Mono<Bundle> addBundle(Bundle bundle) {
        return bundleRepository.save(bundle);
    }

    public Mono<Void> deleteBundle(String idBundle) {
        return bundleRepository.deleteById(idBundle);
    }

    public Mono<Bundle> updateBundle(Bundle bundle) {
        return bundleRepository.save(bundle);
    }
}
