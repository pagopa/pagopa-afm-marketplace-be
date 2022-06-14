package it.pagopa.afm.marketplacebe.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

@RestController()
@RequestMapping(path = "/psps")
@Tag(name = "PSP service API", description = "Everything about PSP")
public class BundleController {

    @GetMapping(
            value = "",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public Mono<Bundles> getBundles(){
        return Mono.just(new Bundles());
    }

}
