# stepmake/Package.make

package-icon=$(outdir)/$(package)-icon.gif

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
	$(PYTHON) $(step-bindir)/package-diff.py --package=$(topdir) $(makeflags)

release: 
	$(PYTHON) $(step-bindir)/release.py --outdir=$(topdir)/$(outdir) --package=$(topdir)

rpm: $(depth)/$(package-icon) dist
	su -c 'rpm -tb $(depth)/$(outdir)/$(distname).tar.gz'

update:
	$(PYTHON) $(step-bindir)/update.py --package=$(topdir) $(makeflags)

xzip:
	$(SHELL) $(step-bindir)/package-zip.sh $(topdir) $(prefix:/%=%)

zip:
	$(SHELL) $(step-bindir)/package-zip32.sh $(topdir)

