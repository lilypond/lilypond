# stepmake/Package.make

deb:
	$(MAKE) -C $(depth)/debian

# urg
#(cd $(depth)/../debian/$(distname); \
#  ln -sf debian DEBIAN; )
# su -c 'dpkg-deb -b $(depth)/../debian/$(distname)'
# urg, why aren't there any decent manual pages for dpkg or rpm?
	(cd $(depth)/../debian; \
	  rm -rf $(distname) $(distname).deb;\
	  tar xz -C $(depth)/../debian -f $(release-dir)/$(distname).tar.gz; \
	  cd $(distname);\
	  dpkg-buildpackage -b -rfakeroot; \
	)

makeflags=$(patsubst %==, %, $(patsubst ---%,,$(patsubst ----%,,$(MAKEFLAGS:%=--%))))

diff:
	$(PYTHON) $(step-bindir)/package-diff.py  --outdir=$(topdir)/$(outdir) --package=$(topdir) $(makeflags)
	-ln -f $(depth)/$(outdir)/$(distname).diff.gz $(patch-dir)

release: 
	$(PYTHON) $(step-bindir)/release.py --outdir=$(topdir)/$(outdir) --package=$(topdir)

# rpm: $(depth)/$(package-icon) dist
rpm: 
	@echo The \"rpm\" target is deprecated.
	@echo Please select the .spec appropriate for your distribution and run
	@echo
	@echo "  rpm -bb SPEC-FILE"
	@echo 
	@echo different SPEC-FILEs may be found in $(depth)/make/out/

#	su -c 'rpm -tb $(depth)/$(outdir)/$(distname).tar.gz'

