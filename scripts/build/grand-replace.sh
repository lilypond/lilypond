#!@BASH@
# note: dash does not work

pytt '(Copyright|\(c\)|\(C\)|@copyright\{\})\s*2008' '\1 2008--2009' $(find . -mindepth 2 -type f | grep -Ev 'out/|out-scons|out-www/|.git/|.scon|#|~' | grep -iv 'change')
pytt '(Copyright|\(c\)|\(C\)|@copyright\{\})\s*([^-]*--)(200[0-8])' '\1 \2\062009' $(find . -mindepth 2 -type f | grep -Ev 'out/|out-scons|out-www/|.git/|.scon|#|~' | grep -iv 'change')
