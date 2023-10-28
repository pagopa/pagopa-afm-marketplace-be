# AFM-Marketplace - Import GEC bundles from NDP configuration DB
the `import-bundles/create-bundels.py` script allow to create and import GEC bundles in `AFM-Marketplace`
by collecting the necessary information from NDP configuration db `NODE4_CFG.ELENCO_SERVIZI`.
The bundles information are stored to db by uploading the Catalogo Dati Informativi `CDI` files from `API-Config`.

## Dependencies
- Python 3.x environment
- module `oracledb`
- module `requests`
## Usage
How to use the script:
```
usage: 
create-bundles.py [-h] \ 
--psp-list PSP_LIST \
--subkey SUBKEY \
--apim-url APIM_URL \
--db-dsn DB_DSN \
--db-port DB_PORT \
--db-user DB_USER \
--db-password DB_PASSWORD

How to use the script

options:
  -h, --help            show this help message and exit
  --psp-list PSP_LIST   comma separated list of PSPs
  --subkey SUBKEY       subkey to invoke apim endpoint
  --apim-url APIM_URL   apim URL
  --db-dsn DB_DSN       oracle db path
  --db-port DB_PORT     oracle db port
  --db-user DB_USER     db user name
  --db-password DB_PASSWORD db password
```
Just an example:
````
python3 create-bundles.py --psp-list PPAYITR1XXX,UNCRITMM \ 
--subkey <SUBKEY> \
--apim-url https://api.uat.platform.pagopa.it/afm/marketplace-service/v1 \
--db-dsn ldbnodosa01/NDPSPCA_SERVICE \
--db-port 1524 \
--db-user <USER> \ 
--db-password <PASSWORD>
````