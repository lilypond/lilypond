#!@SHELL@
# /etc/profile.d/zlily-profile.sh  -- profile additions for Windows
# runs after lilypond-profile.sh

# maybe we should do this once (postinstall) and dump result for
# this login script
POSIX_GS_LIB="$(cygpath -apu ""$GS_LIB"")"
WINDOWS_GS_LIB="$(cygpath -apw ""$POSIX_GS_LIB"")"

POSIX_GS_FONTPATH="$(cygpath -apu ""$GS_FONTPATH"")"
WINDOWS_GS_FONTPATH="$(cygpath -apw ""$POSIX_GS_FONTPATH"")"

POSIX_TEXINPUTS="$(cygpath -apu ""$TEXINPUTS"")"
WINDOWS_TEXINPUTS="$(cygpath -apw ""$POSIX_TEXINPUTS"")"

POSIX_TFMFONTS="$(cygpath -apu ""$TFMFONTS"")"
WINDOWS_TFMFONTS="$(cygpath -apw ""$POSIX_TFMFONTS"")"

POSIX_MFINPUTS="$(cygpath -apu ""$MFINPUTS"")"
WINDOWS_MFINPUTS="$(cygpath -apw ""$POSIX_MFINPUTS"")"

export POSIX_TEXINPUTS POSIX_TFMFONTS POSIX_TFMFONTS
export WINDOWS_TEXINPUTS WINDOWS_TFMFONTS WINDOWS_MFINPUTS

GS_FONTPATH="$WINDOWS_GS_FONTPATH"
GS_LIB="$WINDOWS_GS_LIB"

PATH="/usr/lilypond/bin:$PATH"
PATH="@prefix@/bin:$PATH"

cat <<EOF
Congratulations and welcome to GNU LilyPond.

To get started, visit:
    http://www.lilypond.org/Documentation/windows/out-www/installing.html

If you encounter any problems, visit:
    http://www.lilypond.org/wiki?TroubleshootingWindows

When it all works, head straight to the tutorial:
    http://www.lilypond.org/Documentation/user/out-www/lilypond/Tutorial.html
    
EOF
