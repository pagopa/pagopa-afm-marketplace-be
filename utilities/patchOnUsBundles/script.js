import { CosmosClient } from "@azure/cosmos";

const key = "<cosmos-key>";
const endpoint = "<cosmos-endpoint>";
const containerId = "bundles";
const databaseId = "db";

const client = new CosmosClient({ endpoint, key });
const { database } = await client.databases.createIfNotExists({ id: databaseId });
const { container } = await database.containers.createIfNotExists({ id: containerId });

let numberOfPatch = 0;

async function patchObject(bundleId, partitionKey, onUsValue) {
    const operations =
        [
            { op: 'add', path: '/onus', value: onUsValue },
        ];

    await container.item(bundleId, partitionKey).patch(operations);

    numberOfPatch += 1;
}

async function run() {

    const { resources } = await container.items.readAll().fetchAll();

    console.log("Number of Bundles: ", resources.length);

    for (const bundle of resources) {
        await patchObject(bundle.id, bundle.idPsp, bundle.name.includes("_ONUS"));
    }

    console.log("Number of patches: ", numberOfPatch);
}

run().catch((err) => console.error("Error: ", err));