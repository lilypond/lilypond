.PHONY : texinfo-all-menus-update

default: $(INFO_FILES)

local-WWW: $(addprefix $(outdir)/,$(TEXI_FILES:.texi=.html))

local-doc: $(OUTTXT_FILES)

check-info: texinfo-all-menus-update

## info stuff
local-install: install-info
local-uninstall: uninstall-info
local-install-info:
local-uninstall-info:
install-info: local-install-info
uninstall-info: local-uninstall-info

install-info: $(INFO_FILES)
	$(INFO_INSTALL_COMMAND) local-install

uninstall-info:
	$(INFO_INSTALL_COMMAND) local-uninstall


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


