#!@BASH@

name=install-info-html
version=1.0

all=
index_dir=.

#
# debugging
#
debug_echo=:


#
# print usage
#
help ()
{
	cat << EOF
$name $version
Install HTML info document.

Usage: $name [OPTIONS]... [DOCUMENT-DIR]...

Options:
  -a, --all            assume all subdirectories of index to be DOCUMENT-DIRs
  -d, --dir=DIR        set index directory to DIR (default=.)
  -D, --debug          print debugging info
  -h, --help           show this help text
  -v, --version        show version
EOF
}
 

cleanup ()
{
	$debug_echo "cleaning ($?)..."
}

trap cleanup 0 9 15

#
# Find command line options and switches
#

# "x:" x takes argument
#
options="adhvW:"
#
# ugh, "\-" is a hack to support long options
# must be in double quotes for bash-2.0

while getopts "\-:$options" O
do
	$debug_echo "O: \`$O'"
	$debug_echo "arg: \`$OPTARG'"
	case $O in
		a)
			all=yes
			;;
		D)
		 	[ "$debug_echo" = "echo" ] && set -x
		      	debug_echo=echo
			;;
		h)
      			help;
			exit 0
			;;
		v)
			echo $name $version
			exit 0
			;;
		d)
			index_dir=$OPTARG
			;;
	# a long option!
	-)
		case "$OPTARG" in
			a*|-a*)
				all=yes
				;;
			de*|-de*)
				[ "$debug_echo" = "echo" ] && set -x
				debug_echo=echo
				;;
			h*|-h*)
				help;
				exit 0
				;;
			di*|-di*)
				index_dir="`expr \"$OPTARG\" ':' '[^=]*=\(.*\)'`"
				;;
			version|-version)
				echo $name $version
				exit 0
				;;
			*|-*)
				echo "$0: invalid option -- \"$OPTARG\""
				help;
				exit -1
				;;
		esac
	esac
done
shift `expr $OPTIND - 1`

#
# Input file name
#
if [ -z "$all" -a -z "$1" ]; then
	help
	echo "$name: No HTML documents given"
	exit 2
fi

if [ -n "$all" -a -n "$1" ]; then
	echo "$name: --all specified, ignoring DIRECTORY-DIRs"
fi

if [ -n "$all" ]; then
	document_dirs=`/bin/ls -d1 $index_dir`
else
	document_dirs=$*
fi

index_file=$index_dir/index.html
rm -f $index_file
echo -n "$name: Writing index: $index_file..."

# head
cat >> $index_file <<EOF
<html> 
<title>Info documentation index</title>
<body>
<h1>Info documentation index</h1>
<p>
This is the directory file \`index.html' a.k.a. \`DIR', which contains the
topmost node of the HTML Info hierarchy.
</p>
<ul>
EOF

#list
for i in $document_dirs; do
    cat <<EOF
<li> <a href="$i/index.html">$i</a> (<a href="$i.html">$i as one big page</a>)</li>
EOF
done >> $index_file

# foot
cat >> $index_file <<EOF
</ul>
</body>
</html>
EOF
echo
