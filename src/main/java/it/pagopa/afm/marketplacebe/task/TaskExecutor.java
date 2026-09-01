package it.pagopa.afm.marketplacebe.task;

import java.time.LocalDate;
import org.modelmapper.ModelMapper;

public class TaskExecutor {

  protected ModelMapper modelMapper;
  protected LocalDate now;

  public TaskExecutor() {
    this.modelMapper = new ModelMapper();
    this.now = LocalDate.now();
  }

  public void execute() {
    throw new UnsupportedOperationException();
  }
}
