import { CosmosClient } from "@azure/cosmos";

const key = "<cosmos-key>";
const endpoint = "<cosmos-endpoint>";
const databaseId = "db";

const client = new CosmosClient({ endpoint, key });
const { database } = await client.databases.createIfNotExists({ id: databaseId });

const CONTAINER_ID_BUNDLES = "bundles";
const CONTAINER_ID_VALID_BUNDLES = "validbundles";
const CONTAINER_ID_ARCHIVED_BUNDLES = "archivedbundles";

async function patchObject(container, bundleId, partitionKey, onUsValue) {
    const operations =
        [
            { op: 'add', path: '/onus', value: onUsValue },
        ];

    await container.item(bundleId, partitionKey).patch(operations);
}

async function run(containerId) {
    let numberOfPatch = 0;

    const { container } = await database.containers.createIfNotExists({ id: containerId });

    const { resources } = await container.items.readAll().fetchAll();

    console.log(`Number of ${containerId}: `, resources.length);

    for (const bundle of resources) {
        await patchObject(container, bundle.id, bundle.idPsp, bundle.name.includes("_ONUS"));
        numberOfPatch += 1;
    }

    console.log(`Number of patches for ${containerId}: `, numberOfPatch);
    console.log("\n");
}

await run(CONTAINER_ID_BUNDLES).catch((err) => console.error(`Error container ${CONTAINER_ID_BUNDLES}: `, err));
await run(CONTAINER_ID_VALID_BUNDLES).catch((err) => console.error(`Error container ${CONTAINER_ID_VALID_BUNDLES}: `, err));
await run(CONTAINER_ID_ARCHIVED_BUNDLES).catch((err) => console.error(`Error container ${CONTAINER_ID_ARCHIVED_BUNDLES}: `, err));