data "azuread_application" "apiconfig-fe" {
  display_name = format("pagopa-%s-apiconfig-fe", var.env_short)
}
