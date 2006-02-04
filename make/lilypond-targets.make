#empty


## TODO: fail dist or web if no \version present.
check-version:
	grep -L version $(LY_FILES)
