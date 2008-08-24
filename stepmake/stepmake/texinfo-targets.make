.PHONY : texinfo-all-menus-update

default: $(INFO_FILES)

local-WWW: $(addprefix $(outdir)/,$(TEXI_FILES:.texi=.html))

local-doc: $(OUTTXT_FILES)

check-info: texinfo-all-menus-update

## info stuff
local-install: install-info
local-uninstall: uninstall-info
install-info: local-install-info
uninstall-info: local-uninstall-info

install-info: $(INFO_FILES)
	$(INFO_INSTALL_COMMAND) local-install

uninstall-info:
	$(INFO_INSTALL_COMMAND) local-uninstall

ifeq ($(INFO_FILES),)
local-install-info:
local-uninstall-info:

else # $(INFO_FILES) non empty
# There are two modes for info: with and without images.
ifeq ($(out),www)

# This builds all .info targets with images, in out-www.
# Viewable with a recent Emacs, doing: C-u C-h i out-www/lilypond.info

local-install-info: info
	-$(INSTALL) -d $(DESTDIR)$(infodir)
ifneq ($(patsubst %/local,%,$(DESTDIR)$(prefix)),/usr)
## Can not have absolute symlinks because some binary packages build schemes
## install files in nonstandard root.  Best we can do is to notify the
## builder or packager.
	@echo
	@echo "***************************************************************"
	@echo "Please add or update the LilyPond direntries, do"
	@echo
	@echo "    install-info --info-dir=$(infodir) $(outdir)/$(MAIN_INFO_DOC).info"
	@echo
	@echo "For images in the INFO docs to work, do: "
	@echo
	@echo "    (cd $(infodir) && ln -sfT ../doc/lilypond/html/$(DEST_INFO_IMAGES_SUBDIR) $(INFO_IMAGES_DIR))"
	@echo "or add something like that to the postinstall script."
	@echo
else # installing directly into standard /usr/...
	-$(INSTALL) -d $(DESTDIR)$(infodir)
	$(foreach f,$(INFO_FILES),install-info --remove --info-dir=$(infodir) $(f) ; )true
	install-info --info-dir=$(infodir) $(outdir)/$(MAIN_INFO_DOC).info
	cd $(infodir) && ln -sfT $(webdir)/$(DEST_INFO_IMAGES_SUBDIR) $(INFO_IMAGES_DIR)
endif # installing directly into standard /usr/...

local-uninstall-WWW:
	rm -f $(infodir)/$(INFO_IMAGES_DIR)

else # out!=www

ifneq ($(patsubst %/local,%,$(DESTDIR)$(prefix)),/usr)
## Can not have absolute symlinks because some binary packages build schemes
## install files in nonstandard root.  Best we can do is to notify the
## builder or packager.
local-install-info: info
	-$(INSTALL) -d $(DESTDIR)$(package_infodir)
	@echo
	@echo "***************************************************************"
	@echo "Please add or update the LilyPond direntries, do"
	@echo
	@echo "    install-info --info-dir=$(infodir) out/$(MAIN_INFO_DOC).info"
	@echo
	@echo "For images in the INFO docs to work, do"
	@echo
	@echo "    make out=www install-info "
	@echo
	@echo "and read the extra instructions."
	@echo

local-uninstall-info:
	-rmdir $(DESTDIR)$(package_infodir)

else # installing directly into standard /usr/...
local-install-info: info
	-$(INSTALL) -d $(DESTDIR)$(package_infodir)
	-$(INSTALL) -d $(DESTDIR)$(infodir)
	$(foreach f,$(INFO_FILES),install-info --remove --info-dir=$(infodir) $(f) ; )true
	install-info --info-dir=$(infodir) $(outdir)/$(MAIN_INFO_DOC).info
	@echo
	@echo "***************************************************************"
	@echo "For images in the INFO docs to work, do"
	@echo
	@echo "    make out=www install-info "
	@echo

local-uninstall-info:
	$(foreach f,$(INFO_FILES),install-info --remove --info-dir=$(infodir) $(f) ; )true
	-rmdir $(DESTDIR)$(infodir)
	-rmdir $(DESTDIR)$(package_infodir)

endif # installing into standard /usr/* root

endif # out!=www

endif # $(INFO_FILES) non empty

TEXINFO_ALL_MENUS_UPDATE_EL ='\
  (let ((error nil)\
        (version-control nil))\
      (load-library "texinfo")\
      (texinfo-mode)\
      (texinfo-all-menus-update)\
      (if (buffer-modified-p (current-buffer))\
        (save-buffer))))\
'

# buffer-modified-p is ALWAYS true, even if there were no actual
# changes, so we try setting origal (timestamp) back if there
# were no changes.
#
# ugh: emacs20.7 batch mode is not really batch:
#
# answer `echo q' to the question:
# FILE locked by EMAIL (pid PID): (s, q, p, ?)?
#
texinfo-all-menus-update:
	-$(foreach i, $(TEXINFO_SOURCES), echo q | emacs --batch --no-site-file $(i) --eval $(TEXINFO_ALL_MENUS_UPDATE_EL); )
	$(foreach i, $(sort $(TEXINFO_SOURCES)), if diff -u $(i)~ $(i); then mv $(i)~ $(i);  fi && ) true

local-help: local-texinfo-help

local-texinfo-help:
	@echo -e "\
  info [out=www]  update Info documentation (use \`out=www' for having images)\n\
  install-info [out=www]   install Info documentation (idem)\n\
  texinfo-all-menus-update update node menus in Texinfo source files (use with caution)\n"

