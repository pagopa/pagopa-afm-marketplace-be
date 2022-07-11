package it.pagopa.afm.marketplacebe.model.offer;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@Builder
public class CiFiscalCodeList {

    List<String> ciFiscalCodeList;
}
