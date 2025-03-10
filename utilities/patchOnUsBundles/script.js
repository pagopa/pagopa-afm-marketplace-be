import { CosmosClient } from "@azure/cosmos";

const key = "<cosmos-key>";
const endpoint = "<cosmos-endpoint>";
const databaseId = "db";

const client = new CosmosClient({ endpoint, key });
const { database } = await client.databases.createIfNotExists({ id: databaseId });

const CONTAINER_ID_BUNDLES = "bundles";
const CONTAINER_ID_VALID_BUNDLES = "validbundles";
const CONTAINER_ID_ARCHIVED_BUNDLES = "archivedbundles";

const ONUS_SUFFIX = "_ONUS";

async function patchObjects(container, bundles) {
    const operations = bundles.map(bundle => ({
        "operationType": "Patch",
        "id": bundle.id,
        "partitionKey": bundle.idPsp,
        "resourceBody": { operations: [{ op: 'add', path: '/onus', value: bundle.name.includes(ONUS_SUFFIX)}] }
    }));
    const response = await container.items.bulk(operations);

    return response.filter(el => el.statusCode === 200).length;
}

async function run(containerId) {
    let numberOfPatch = 0;

    const { container } = await database.containers.createIfNotExists({ id: containerId });

    const { resources } = await container.items.readAll().fetchAll();

    console.log(`Number of ${containerId}: `, resources.length);

    for (let i = 0; i <= resources.length; i += 100) {
        const patched = await patchObjects(container, resources.slice(i, i+100));
        numberOfPatch += patched;
    }

    console.log(`Number of patches for ${containerId}: `, numberOfPatch);
    console.log("\n");
}

await run(CONTAINER_ID_BUNDLES).catch((err) => console.error(`Error container ${CONTAINER_ID_BUNDLES}: `, err));
await run(CONTAINER_ID_VALID_BUNDLES).catch((err) => console.error(`Error container ${CONTAINER_ID_VALID_BUNDLES}: `, err));
await run(CONTAINER_ID_ARCHIVED_BUNDLES).catch((err) => console.error(`Error container ${CONTAINER_ID_ARCHIVED_BUNDLES}: `, err));