import json
import argparse
import os
from datetime import datetime
import oracledb
import requests
from requests.exceptions import HTTPError


def update_bundles(psp_list):
    print("[update_bundles] | starting to update bundles [{}]".format(psp_list))
    if len(psp_list) == 0:
        print("[update_bundles] | empty psp list provided")
        exit()

    for psp in psp_list:
        print("[update_bundles] | retrieving existing bundles for psp [{}]".format(psp))
        # get list of old id bundles to delete 
        afm_bundles = get_bundles_by_psp(psp)
        if afm_bundles is None:
            continue
        for bundle_id in afm_bundles:
            delete_bundle(psp, bundle_id)
        print("[update_bundles] | no more valid bundle for psp [{}] are present".format(psp))

        # get new bundles to insert
        print("[update_bundles] | reading new bundle to insert for psp [{}]".format(psp))
        bundles = get_bundles_from_db(psp)
        print("[update_bundles] | found {} bundles to insert".format(len(bundles)))
        for bundle in bundles:
            b = create_bundle(bundle)
            insert_bundle(psp, b)

        print("update_bundles] | bundles created for psp [{}]".format(psp))

    # reload afm configuration
    print("[update_bundles] | reloading configuration")
    reload_configuration()
    print("[update_bundles] | configuration reloaded")


def insert_bundle(id_psp, bundle):
    print("[insert_bundle] | trying to insert bundle -> id_psp[{}]".format(id_psp))

    api_url = "{}/psps/{}/bundles".format(apim_url, id_psp)
    headers = {'Ocp-Apim-Subscription-Key': subkey, 'content-type': 'application/json'}

    try:
        print("[insert_bundle] | calling insert api [{}]".format(api_url))
        response = requests.post(api_url, headers=headers, json=bundle)
        print("[insert_bundle] | response code [{}]".format(response.status_code))
        response.raise_for_status()
        json_response = response.json()
        bundle_id = json_response['idBundle']
        print("[insert_bundle] | bundle [{}] created".format(bundle_id))

        print("[insert_bundle] | bundle details {}".format(json.dumps(json_response, indent=4)))

    except HTTPError as http_err:
        print(f'[insert_bundle] | HTTP error occurred: {http_err}')
    except Exception as err:
        print(f'[insert_bundle] | Generic error occurred: {err}')


def delete_bundle(id_psp, bundle_id):
    print("[delete_bundle] | trying to delete bundle id_psp[{}] idBundle[{}]".format(id_psp, bundle_id))

    api_url = "{}/psps/{}/bundles/{}".format(apim_url, id_psp, bundle_id)
    headers = {'Ocp-Apim-Subscription-Key': subkey}

    try:
        print("[delete_bundle] | calling delete api [{}]".format(api_url))
        response = requests.delete(api_url, headers=headers)
        print("[delete_bundle] | response code [{}]".format(response.status_code))
        response.raise_for_status()

        print("[delete_bundle] | bundle id_psp[{}] idBundle[{}] deleted from afm-marketplace".format(id_psp, bundle_id))

    except HTTPError as http_err:
        print(f'[delete_bundle] | HTTP error occurred: {http_err}')
    except Exception as err:
        print(f'[delete_bundle] | Generic error occurred: {err}')


def reload_configuration():
    print("[reload_configuration] | trying to reload configuration")

    api_url = "{}/configuration".format(apim_url)
    headers = {'Ocp-Apim-Subscription-Key': subkey}

    try:
        print("[reload_configuration] | calling reload configuration api [{}]".format(api_url))
        response = requests.get(api_url, headers=headers)
        print("[reload_configuration] | response code [{}]".format(response.status_code))
        response.raise_for_status()

        print("[reload_configuration] | configuration has been reloaded")

    except HTTPError as http_err:
        print(f'[delete_bundle] | HTTP error occurred: {http_err}')
    except Exception as err:
        print(f'[delete_bundle] | Generic error occurred: {err}')


def get_bundles_from_db(psp):
    print("[get_bundles_from_db] | Creating connection...")
    connection = oracledb.connect(
        dsn=db_dsn,
        port=int(db_port),
        user=db_user,
        password=db_password
    )
    print("[get_bundles_from_db] | successfully connected to oracle database [{}]".format(db_dsn))

    print("[get_bundles_from_db] | creating cursor...")
    cursor = connection.cursor()
    print("[get_bundles_from_db] | cursor created")

    bundles = []
    i = 0
    sql_query = str("""
                    SELECT DISTINCT  
                        PSP_ID AS idPsp,
                        PSP_RAG_SOC AS ragioneSociale,
                        CANALE_ID AS idChannel,
                        INTM_ID AS idBrokerPsp,
                        FLUSSO_ID AS idCdi,
                        CODICE_ABI AS abi,
                        PSP_FLAG_BOLLO AS digitalStamp,
                        NOME_SERVIZIO AS name,
                        INF_DESC_SERV AS descrizione, 
                        COSTO_FISSO AS paymentAmount, 
                        IMPORTO_MINIMO AS minPaymentAmount,
                        IMPORTO_MASSIMO AS maxPaymentAmount,
                        TIPO_VERS_COD AS paymentType 
                    FROM 
                        NODO4_CFG.ELENCO_SERVIZI es
                    WHERE 
                        TIPO_VERS_COD = 'CP' 
                        AND CARRELLO_CARTE = 'Y'
                        AND CANALE_APP = 'N'
                        AND CODICE_LINGUA = 'IT'
                        AND FLAG_IO = 'Y' 
                        AND PSP_ID NOT LIKE 'CHARITY%'
                        AND PSP_ID NOT IN ('ABI14156')
                        AND PSP_ID = '{}' 
                        AND CODICE_CONVENZIONE IS NULL 
                    ORDER BY PSP_RAG_SOC""").format(psp)

    print("[get_bundles_from_db] | executing query [{}]".format(sql_query))

    for row in cursor.execute(sql_query):
        bundles.append(row)
        i += 1

    connection.close()

    print("[get_bundles_from_db] | Found [{}] bundles that match the query".format(i))
    return bundles


def create_bundle(item):
    print("[create_bundle] | creating bundle")
    bundle = {
        "idPsp": str(item[0]),
        "idChannel": str(item[2]),
        "idBrokerPsp": str(item[3]),
        "idCdi": str(item[4]),
        "abi": "07601" if item[5] == "POSTE" else str(item[5]),
        "digitalStamp": bool(False) if item[6] == "N" else bool(True),
        "digitalStampRestriction": bool(False),
        "name": str(item[1]),
        "pspBusinessName": str(item[1]),
        "description": str(item[8]),
        "paymentAmount": int(float(item[9]) * 100),
        "minPaymentAmount": int(float(item[10]) * 100),
        "maxPaymentAmount": int(float(item[11]) * 100),
        "paymentType": str(item[12]),
        "touchpoint": "CHECKOUT",
        "type": "GLOBAL",
        "transferCategoryList": None,
        "validityDateFrom": datetime.today().strftime('%Y-%m-%d'),
        "validityDateTo": None
    }

    print("[create_bundle] | bundle created")

    return bundle


# Return an array of all bundle id defined for a specific psp
def get_bundles_by_psp(id_psp):
    print("[get_bundles_by_psp] | trying to retrieve bundles id_psp[{}]".format(id_psp))
    try:
        api_url = "{}/psps/{}/bundles".format(apim_url, id_psp)
        headers = {'Ocp-Apim-Subscription-Key': subkey}

        print("[get_bundles_by_psp] | calling get bundle by psp api [{}]".format(api_url))
        response = requests.get(api_url, headers=headers)
        print("[get_bundles_by_psp] | response code [{}]".format(response.status_code))
        response.raise_for_status()
        json_response = response.json()

        id_bundles = []
        for value in json_response['bundles']:
            id_bundles.append(value['idBundle'])

        print("[get_bundles_by_psp] | found bundle from psp [{}] from db".format(id_psp))
        return id_bundles

    except HTTPError as http_err:
        print(f'[get_bundles_by_psp] | HTTP error occurred: {http_err}')
    except Exception as err:
        print(f'[get_bundles_by_psp] | Generic error occurred: {err}')


# reading input parameters
print("[main] | parsing input parameters")
parser = argparse.ArgumentParser(description="How to use the script", formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('--psp-list', required=True, help='comma separated list of PSPs')
parser.add_argument('--subkey', required=True, help='subkey to invoke apim endpoint')
parser.add_argument('--apim-url', required=True, help='apim URL')
parser.add_argument('--db-dsn', required=True, help='oracle db path')
parser.add_argument('--db-port', required=True, help='oracle db port')
parser.add_argument('--db-user', required=True, help='db user name')
parser.add_argument('--db-password', required=True, help='db password')
args = parser.parse_args()

# get basedir
dirname = os.path.dirname(__file__)
psps = str(args.psp_list).split(',')
subkey = str(args.subkey)
apim_url = str(args.apim_url)
db_dsn = str(args.db_dsn)
db_port = str(args.db_port)
db_user = str(args.db_user)
db_password = str(args.db_password)

print("[main] | input parameters loaded")
# get bundles from NODO4_CFG.ELENCO_SERVIZI table
update_bundles(psps)
