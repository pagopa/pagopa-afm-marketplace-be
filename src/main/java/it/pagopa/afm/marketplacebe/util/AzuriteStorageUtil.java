package it.pagopa.afm.marketplacebe.util;


import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import lombok.AllArgsConstructor;

@AllArgsConstructor
public class AzuriteStorageUtil {

    private final boolean debugAzurite = Boolean.parseBoolean(System.getenv("DEBUG_AZURITE"));

    private String storageConnectionString;
    private String flowsTable;
    private String flowsQueue;
    private String containerBlob;

    // Create a new table
//    public void createTable() throws URISyntaxException, InvalidKeyException, StorageException {
//        if (debugAzurite) {
//            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
//            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
//            TableRequestOptions tableRequestOptions = new TableRequestOptions();
//            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster
//            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
//            CloudTable table = cloudTableClient.getTableReference(flowsTable);
//
//            table.createIfNotExists();
//        }
//    }

    // Create a new queue
//    public void createQueue() throws URISyntaxException, InvalidKeyException, StorageException {
//        if (debugAzurite) {
//            CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
//                    .getQueueReference(flowsQueue);
//            queue.createIfNotExists();
//        }
//    }

    // Create a new blob
    public void createBlob() throws NullPointerException {
        if (debugAzurite) {
            BlobServiceClient blobServiceClient = new BlobServiceClientBuilder()
                    .connectionString(this.storageConnectionString).buildClient();
            BlobContainerClient container = blobServiceClient.getBlobContainerClient(containerBlob);
            if (!container.exists()) {
                blobServiceClient.createBlobContainer(containerBlob);
            }
        }
    }
}
