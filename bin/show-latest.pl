#!@PERL@
# -*-Perl-*-

$LILYPOND_SOURCEDIR="$ENV{LILYPOND_SOURCEDIR}";
$LILYPOND_ROOTDIR=`cd $LILYPOND_SOURCEDIR/..; pwd`;
chop ($LILYPOND_ROOTDIR);
$reldir="$LILYPOND_ROOTDIR/releases";

use FileHandle;
use Getopt::Long;

sub cmpver 
{ 	
    my(@a)= split( /\./, $a);
    my(@b)= split( /\./, $b);
    
    for $i (0,1,2) {
	return $a[$i] <=> $b[$i] if ($a[$i] != $b[$i]);
    }
    return $a cmp $b;
}

my @versions;
open LS, "ls -1 $reldir|";

GetOptions( 'print', 'news', 'latest');


while (<LS>) {
    $_ =~ /lilypond-([^.]+\.[^.]+\.[^.]+).tar.gz/;
    push @versions, $1;
}


@versions = sort cmpver @versions;
my $last= (pop @versions);


if (  $opt_print ) {
    print $last . "\n";
}

if ( $opt_latest) {
    system "rm $reldir/zZ*";
    system  "> $reldir/zZ_LATEST_IS_$last";
}
if ( $opt_news ) {
    open NEWS, "tar --to-stdout  -zxf $reldir/lilypond-$last.tar.gz lilypond-$last/NEWS |";
    input_record_separator NEWS "****";
    $desc = <NEWS>;
    chop ($desc);
    close NEWS;

    print $desc;
}
