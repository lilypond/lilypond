#! @BASH@
# note: dash does not work

pytt '(Copyright|\(c\)|\(C\)|@copyright\{\})\s*2006' '\1 2006--2007' $(find . -mindepth 2 -type f | grep -Ev 'out/|out-scons|.git/|.scon|#|~|' | grep -iv 'change')
pytt '(Copyright|\(c\)|\(C\)|@copyright\{\})\s*([^-]*--)(200[0-6])' '\1 \2\062007' $(find . -mindepth 2 -type f | grep -Ev 'out/|out-scons|.git/|.scon|#|~' | grep -iv 'change')
