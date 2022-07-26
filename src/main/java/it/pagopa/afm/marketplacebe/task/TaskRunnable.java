package it.pagopa.afm.marketplacebe.task;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class TaskRunnable implements Runnable {

    private TaskExecutor taskExecutor;

    public TaskRunnable(TaskExecutor taskExecutor) {
        this.taskExecutor = taskExecutor;
    }

    @Override
    public void run() {
        taskExecutor.execute();
    }
}
