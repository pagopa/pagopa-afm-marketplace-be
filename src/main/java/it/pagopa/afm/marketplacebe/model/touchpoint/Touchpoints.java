package it.pagopa.afm.marketplacebe.model.touchpoint;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import java.util.List;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class Touchpoints {

  @JsonProperty("touchpoints")
  @Schema(required = true)
  @NotNull
  @Valid
  private List<Touchpoint> touchpointList;

  @Schema(required = true)
  @NotNull
  @Valid
  private PageInfo pageInfo;
}
