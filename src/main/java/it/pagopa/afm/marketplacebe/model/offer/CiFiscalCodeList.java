package it.pagopa.afm.marketplacebe.model.offer;

import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class CiFiscalCodeList {

    List<String> ciFiscalCodeList;
}
