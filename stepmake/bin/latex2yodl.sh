#!/bin/sh
# latex2yodl.sh -- one time convert helper

if test $# -lt 1
then
   echo "Usage: latex2yodl FILE..."; 
   exit 2
fi
for i in $*
do
    echo $i
    base=`basename $base .latex`
    base=`basename $base .tex`
    base=`basename $i .doc`
    yo=$base.yo
    rm -f $yo
    perl -pe '
    	s/([a-zA-Z][a-zA-Z]*)\~/bind($1)/g;
    	s/%(.*)/COMMENT($1)/g;
    	s/\$\^{([^}]*)}$/ sups($1)/g;
    	s/\$\_{([^}]*)}$/ subs($1)/g;
    	s/\$\^(.)$/ sups($1)/g;
    	s/\$\_(.)$/ subs($1)/g;
    	s/\\appendix/appendix()/g;
    	s/\\footnote{([^}]*)}/ footnote($1)/g;
    	s/\\cite{([^}]*)}/ cite($1)/g;
    	s/(\\marginpar{[^}]*})/latexcommand($1)/g;
    	s/\\chapter *{([^}]*)}/chapter($1)/g;
    	s/\\chapter *\* *{([^}]*)}/nchapter($1)/g;
    	s/\\section *{([^}]*)}/sect($1)/g;
    	s/\\section *\* *{([^}]*)}/nsect($1)/g;
    	s/\\subsection *{([^}]*)}/subsect($1)/g;
    	s/\\subsection *\* *{([^}]*)}/nsubsect($1)/g;
    	s/\\begin{itemize}.*/itemize(/g;
    	s/\\item *{([^}]*)}/dit($1)/g;
    	s/\\item *[[]([^]]*)[]]/dit($1)/g;
    	s/\\item */it()/g;
    	s/\\(caption{[^}]*})/latexcommand(XXX$1)/g;
    	s/\\(begin{figure}.*)/latexcommand(XXX$1)/g;
    	s/\\(end{figure}.*)/latexcommand(XXX$1\n)/g;
    	s/\\begin{mudela}[[]([^]]*)[]]/mudela($1)(/g;
    	s/\\begin{mudela}.*/mudela()(/g;
    	s/\\end{mudela}/)/g;
    	s/\\(begin{table}.*)/latexcommand(XXX$1)/g;
    	s/\\(end{table}.*)/latexcommand(XXX$1)/g;
    	s/\\begin{tabular}{(.*)}/table(ncol)($1)(/g;
    	s/\\begin{verbatim}.*/verb(/g;
    	s/\\begin{([^}]*)}/$1(/g;
    	s/\\end{([^}]*)}/)/g;
    	s/{\\em ([^}]*)}/em($1)/g;
    	s/\\emph{([^}]*)}/em($1)/g;
    	s/\\ref{([^}]*)}/ref($1)/g;
    	s/\\texttt{([^}]*)}/code($1)/g;
    	s/\\file{([^}]*)}/file($1)/g;
    	s/\\label{([^}]*)}/label($1)/g;
    	s/\\verb[+]([^+]*)[+]/code($1)/g;
    	s/\\verb[|]([^|]*)[|]/code($1)/g;
	s/XXX/\\/g;
	' $i > $yo
done

