#!@SHELL@
# use kpsewhich?
# maybe (optionally) (not) clean stuff from other versions, ie, don't clean
#     /var/spool/texmf/tfm/lilypond/<NOT-OUR-VERSION>/
# ?

VERSION="@TOPLEVEL_VERSION@

case  $# in
0) 
    WHAT="" ;;
1)
    WHAT=$1;;
esac

dirs=".
/var/lib/texmf
/var/spool/texmf
/var/tmp/texfonts
/var/texfonts
"

for i in $dirs; do
	if [ -d "$i" ]; then
		TEXDIRS="$TEXDIRS $i"
	fi
done

if [ -z "$TEXDIRS" -o "$TEXDIRS" = "." ]; then
    TEXDIRS=". /var"
fi

# remove possibly stale .pk/.tfm files 
FILES=`find $TEXDIRS -name "feta*$WHAT*tfm" -or -name "feta*$WHAT*pk"`

echo removing $FILES
rm  -f $FILES /tmp/cleaning-font-dummy
