#!@BASH@
# note: dash does not work

pytt '(Copyright|\(c\)|\(C\)|@copyright\{\})\s*2007' '\1 2007--2008' $(find . -mindepth 2 -type f | grep -Ev 'out/|out-scons|out-www/|.git/|.scon|#|~' | grep -iv 'change')
pytt '(Copyright|\(c\)|\(C\)|@copyright\{\})\s*([^-]*--)(200[0-7])' '\1 \2\062008' $(find . -mindepth 2 -type f | grep -Ev 'out/|out-scons|out-www/|.git/|.scon|#|~' | grep -iv 'change')
