#############################
## Product AFM Marketplace ##
#############################
locals {
  apim_afm_marketplace_service_api = {
    display_name          = "AFM Marketplace pagoPA - marketplace of advanced fees management service API"
    description           = "Marketplace API to support advanced fees management service"
    path                  = "afm/marketplace-service"
    product_id            = "afm-marketplace"
    subscription_required = true
    service_url           = null
  }
}

resource "azurerm_api_management_api_version_set" "api_afm_marketplace_api" {

  name                = format("%s-afm-marketplace-service-api", var.env_short)
  resource_group_name = local.pagopa_apim_rg
  api_management_name = local.pagopa_apim_name
  display_name        = local.apim_afm_marketplace_service_api.display_name
  versioning_scheme   = "Segment"
}


module "apim_api_afm_marketplace_api_v1" {
  source = "./.terraform/modules/__v3__/api_management_api"

  name                  = format("%s-afm-marketplace-service-api", local.project)
  api_management_name   = local.pagopa_apim_name
  resource_group_name   = local.pagopa_apim_rg
  product_ids           = [local.apim_afm_marketplace_service_api.product_id]
  subscription_required = local.apim_afm_marketplace_service_api.subscription_required
  version_set_id        = azurerm_api_management_api_version_set.api_afm_marketplace_api.id
  api_version           = "v1"

  description  = local.apim_afm_marketplace_service_api.description
  display_name = local.apim_afm_marketplace_service_api.display_name
  path         = local.apim_afm_marketplace_service_api.path
  protocols    = ["https"]
  service_url  = local.apim_afm_marketplace_service_api.service_url

  content_format = "openapi"
  content_value = templatefile("../openapi/openapi.json", {
    host = local.apim_hostname
  })

  xml_content = templatefile("./api/marketplace-service/v1/_base_policy.xml", {
    hostname = local.afm_hostname
  })
}