.PHONY : texinfo-all-menus-update

default: $(INFO_FILES)

ifeq ($(out),www)
local-WWW-1: $(XREF_MAPS_FILES)

local-WWW-2: $(OUT_CSS_FILES)
endif

local-doc:  $(OUTTXT_FILES)

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

ifneq ($(COPY_INFO_IMAGES),)
# only Info docs are installed (not HTML nor PDF),
# so images should be copied

install-info-images:
# remove $(infodir)/$(INFO_IMAGES_DIR) in case it is a symlink
	-rm -f $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR)
	$(INSTALL) -d $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR)
	rsync -r --include '[0-9a-f][0-9a-f]' --include '*.png' --exclude '*' $(outdir)/ $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR)/

uninstall-info-images:
	rm -f $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR) || rm -rf $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR)

else # if HTML and PDF docs are installed too, symlink image directories
install-info-images: uninstall-info-images
	cd $(DESTDIR)$(infodir) && ln -sf $$($(PYTHON) $(buildscript-dir)/relative $(DESTDIR)$(webdir)/$(DEST_INFO_IMAGES_SUBDIR)) $(INFO_IMAGES_DIR)

uninstall-info-images:
	rm -f $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR) || rm -rf $(DESTDIR)$(infodir)/$(INFO_IMAGES_DIR)
endif # copying info images


ifneq ($(patsubst %/local,%,$(DESTDIR)$(prefix)),/usr)
## install-info can't do all its job for binary packages' build systems.
## Best we can do is to notify the builder or packager.
local-install-info: info
	-$(INSTALL) -d $(DESTDIR)$(infodir)
	@echo
	@echo "***************************************************************"
	@echo "Please add or update the LilyPond direntries,"
	@echo "do or add in the postinstall script"
	@echo
	@echo "    install-info --info-dir=$(infodir) $(DESTDIR)$(infodir)/$(MAIN_INFO_DOC).info"
	@echo
	$(MAKE) install-info-images

local-uninstall-info: uninstall-info-images
	-rmdir $(DESTDIR)$(infodir)

else # installing directly into standard /usr/...
local-install-info: info
	-$(INSTALL) -d $(DESTDIR)$(infodir)
	$(foreach f,$(INFO_FILES),install-info --remove --info-dir=$(infodir) $(f) ; )true
	install-info --info-dir=$(infodir) $(outdir)/$(MAIN_INFO_DOC).info
	$(MAKE) install-info-images

local-uninstall-info: uninstall-info-images
	$(foreach f,$(INFO_FILES),install-info --remove --info-dir=$(infodir) $(f) ; )true

endif # installing directly into standard /usr/...

else # out!=www

ifneq ($(patsubst %/local,%,$(DESTDIR)$(prefix)),/usr)
## install-info can't do all his job for binary packages build systems.
## Best we can do is to notify the builder or packager.
local-install-info: info
	-$(INSTALL) -d $(DESTDIR)$(infodir)
	@echo
	@echo "***************************************************************"
	@echo "Please add or update the LilyPond direntries, do"
	@echo
	@echo "    install-info --info-dir=$(infodir) $(DESTDIR)$(infodir)/$(MAIN_INFO_DOC).info"
	@echo
	@echo "To compile Info documentation with images, please read"
	@echo "Application Usage document, section \"Building documentation\"."
	@echo

local-uninstall-info:
	-rmdir $(DESTDIR)$(infodir)

else # installing directly into standard /usr/...
local-install-info: info
	-$(INSTALL) -d $(DESTDIR)$(infodir)
	$(foreach f,$(INFO_FILES),install-info --remove --info-dir=$(infodir) $(f) ; )true
	install-info --info-dir=$(infodir) $(outdir)/$(MAIN_INFO_DOC).info
	@echo
	@echo "***************************************************************"
	@echo "To compile Info documentation with images, do from top of the build tree"
	@echo
	@echo "    make doc"
	@echo
	@echo "which builds documentation in all formats; to build only Info documentation, do"
	@echo
	@echo "    make info"
	@echo
	@echo "To list all available targets, do"
	@echo
	@echo "    make help"
	@echo
	@echo "For details, please read Application Usage document, section \"Building documentation\"."
	@echo

local-uninstall-info:
	$(foreach f,$(INFO_FILES),install-info --remove --info-dir=$(infodir) $(f) ; )true

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

