# Save a copy of alias list for preservation sake of package at time of creation
# Possible future backup in case internet access is down for PANGO source

pangoro_cov_alias <- fetch_pango_alias()
usethis::use_data(pangoro_cov_alias, overwrite = TRUE, internal = TRUE)
