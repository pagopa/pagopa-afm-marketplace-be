package it.pagopa.afm.marketplacebe.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping(path = "/psp")
@Tag(name = "PSP service API", description = "Everything about PSP")
public class PspController {
}
