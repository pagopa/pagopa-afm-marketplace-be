package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.model.request.Requests;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class BundleRequestService {

    @Autowired
    BundleRequestRepository bundleRequestRepository;

    public Requests getRequests(String idPsp, Integer size, String cursor, String ciFiscalCode) {
        var requests = bundleRequestRepository.findByIdPsp(idPsp);
        return Requests.builder()
                .requestsList(requests.collectList().block())
                .build();
    }
}
