#!/bin/sh
# pod2yodl.sh -- one time convert helper

if test $# -lt 1
then
   echo "Usage: pod2yodl FILE..."; 
   exit 2
fi
for i in $*
do
    echo $i
    base=`basename $i .in`
    base=`basename $base .pod`
    yo=$base.yo
    rm -f $yo
    perl -pe '
    	s/=head1 *(.*)/nsect($1)/g;
    	s/=head2 *(.*)/nsubsect($1)/g;
    	s/=head3 *(.*)/nsubsubsect($1)/g;
    	s/=over.*(.*)/itemize(/g;
    	s/=item *\*/it()/g;
    	s/=item *(.*)/dit($1)/g;
    	s/=back.*/)/g;
    	s/=begin.*//g;
    	s/=end.*//g;
    	s/C<([^>]*)>/code($1)/g;
    	s/F<([^>]*)>/file($1)/g;
    	s/B<([^>]*)>/bf($1)/g;
    	s/I<([^>]*)>/em($1)/g;
    	s/<[Aa] *[Hh][Rr][Ee][Ff]=\"*([^>\"])\"([^<]*)<\/[Aa]>/ url($2)($1)/g;
    	s/<[Aa] *[Hh][Rr][Ee][Ff]=\"*([^>\"])\"([^<]*)/ url($2)($1)/g;
    	s/<[Aa] *[Hh][Rr][Ee][Ff]=\"*([^>\"])\"/ url()($1)/g;
    	s/.*<\/[Aa]>/($1)/g;
    	s/<(.*@.*)>/email($1)/g;
    	s/(http:\/\/[^ ]*)/ lurl($1)/g;
    	s/(ftp:\/\/[^ ]*)/ lurl($1)/g;
    	s/@([A-Z_]*)@/verbinclude($1.in)/g;
	' $i > $yo
done

