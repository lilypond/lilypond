#!@SHELL@
# walk.sh
# ugh
# print reversed relative path to $1

function base ()
{
    expr "$1" : "\(/[^/]*\)"
}

function unbase ()
{
    expr "$1" : "/[^/]*\(.*\)"
}

function walk ()
{
    from=`(cd "$1" && pwd)`
    to=`pwd`
    t=`base "$to"`
    f=`base "$from"`
    while [ -n "$t" -a "$t" = "$f" ]; do
	to=`unbase "$to"`
	from=`unbase "$from"`
	t=`base "$to"`
	f=`base "$from"`
    done
    i=`echo $from | sed -e 's/[^/]\+/../g'`
    if expr "$from" : '/' > /dev/null 2>&1; then
    	c=..
    fi
    echo $c$i$to
}

walk $1

