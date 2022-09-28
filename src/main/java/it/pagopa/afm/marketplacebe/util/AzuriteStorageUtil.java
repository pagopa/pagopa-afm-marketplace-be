package it.pagopa.afm.marketplacebe.util;


import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import lombok.AllArgsConstructor;

@AllArgsConstructor
public class AzuriteStorageUtil {

    private final boolean debugAzurite = Boolean.parseBoolean(System.getenv("DEBUG_AZURITE"));

    private String storageConnectionString;
    private String containerBlob;

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
