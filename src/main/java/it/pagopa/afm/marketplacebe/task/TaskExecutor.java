package it.pagopa.afm.marketplacebe.task;

import org.modelmapper.ModelMapper;

import java.time.LocalDate;

public class TaskExecutor {

    protected ModelMapper modelMapper;
    protected LocalDate now;

    public TaskExecutor() {
        this.modelMapper = new ModelMapper();
        this.now = LocalDate.now();
    }

    public void execute() {}
}
