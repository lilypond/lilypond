#!@PERL@ -w
# -*-perl-*-

=head1 TODO

    detect \lyrics and \melodic, and do substitution accordingly.
    count <> and {} ?

Ugh . Perl sux. Anybody for Python?
    
=cut    



#
# version of "supporting" engine, not mudela conversions.
# 




$convert_mudela_version = "0.1.2";

use Getopt::Long;


sub version_compare
{
    local ($a,$b)=@_;
    return &cmpver;
}
    

sub  cmpver 
{ 	
	my(@a)= split /\./,$a;
	my(@b)= split /\./,$b;

	for $i (0,1,2) {
	    return $a[$i] <=> $b[$i] if ($a[$i] != $b[$i]);
	}
	return $a cmp $b;
}

sub version_string_conv
{
    my ($from_version, $to_version) = @_;
    s/\version \"$from_version\"/\version \"$to_version\"/g;
}

################################################################

sub no_conv
{
}
 
sub convert_0_0_52_to_0_0_53
{

    s/include \"/$1\\include \"/g;
}

  
sub convert_0_0_54_to_0_0_55
{
    s/%{/% {/g;
}

  
sub convert_0_0_53_to_0_0_54
{
    print STDERR "Not smart enough to convert \\transpose\n"    if (/\\transpose/) ;
}

# we-re not at 58 yet, but this is at least one of the rules
sub convert_0_0_55_to_0_0_56
{
    s/\"\|\|\"/\"|.\"/g;
}

sub convert_0_0_56_to_0_0_57
{
    s/\(([ \]\[|\t-\.>]|\\[<!>a-z]+)*\)/\~ $1/g;
}

sub convert_0_0_57_to_0_0_58
{
    s/\[ *([^\[\]]*)\] *([1-9]*) *\/ *([1-9]*)/[$2\/$3 $1]1\/1/g;
}

sub convert_0_0_58_to_0_0_59
{
    die "Not smart enough to convert 0.0.58 to 0.0.59\n";
}

sub convert_0_0_59_to_0_0_60
{
    s/(\\unitspace [0-9.mcptin\\ ]+|\\geometric [0-9.]+|\\width [0-9.mcp\\tin]+)/$1;/g;
    s/(\\output \"[^\"]+\")/$1;/;
    s/(\\tempo  [0-9: ]+)/$1;/;
}

sub convert_0_0_60_to_0_0_61
{
    s/(\\unitspace|\\geometric|\\width)/$1=/g;
   
}

sub convert_0_1_0_to_0_1_1
{
    s/\\tempo (.*):(.*);/\\tempo $1 = $2;/g
}

sub convert_0_1_2_to_0_1_3
{
    s/\\stem *(\\up|1) *;/\\stemup/g;
    s/\\stem *(\\down|-1) *;/\\stemdown/g;
    s/\\stem *0 *;/\\stemboth/g;
    s/\\hshift ([^;]+) *;/\\property Voice.hshift = $1/g;
}

my $header_b = 0;

sub generic_conversion_scan
{
    if (/\\header *\{/)
    {
	$header_b = 1;
    }
    if ($header_b && /^ *\}/)
    {
	$header_b = 0;
    }
}
sub convert_0_1_4_to_0_1_5
{
    s/([<{]) *\\id "Piano" (.+);/\\type Grandstaff = $3 $1/;    
    s/([<{]) *\\id (.+) (.+);/\\type $2 = $3 $1/;
}

       
sub convert_0_1_5_to_0_1_6
{
    s/< *\\multi (.*);/\\multi $1 </;
}

sub convert_0_1_6_to_0_1_7
{
    if ($header_b) 
    {
	s/^([a-zA-z]+)[ \t]+(.*)$/$1 =\t \"$2\";/;
	s/^([ \t])+(.*)$/$1 \"$2\";/;
    }
}  

sub convert_0_1_7_to_0_1_8
{
    s/\\plet *1 *\/ *1 *;/\\]/;    
    s/\\plet *([1-9][0-9]*) *\/ *([2-9][0-9]*) *;/\\[$1\/$2/;    
}  

sub convert_0_1_8_to_0_1_9
{
# sticky plet shorthand...
#  print "introduced plet and finger shorthands...\n";
}

sub convert_0_1_9_to_0_1_10
{
    s/Grandstaff/Grand_staff/;    
}

sub convert_0_1_10_to_0_1_14
{
    while ( /([ \n\t\]\[<>()])\'+[a-zA-Z]/ )
    {
	s/([ \n\t\[<>()\]])\'(\'*[a-zA-Z]+)/$1$2,/g;
    }

}
###############################################################

sub    last_conversion
{
    my @v = &versions;
    return pop @v;
}
sub identify
{
    
    print STDERR "This is convert-mudela " . $convert_mudela_version . 
	" (up to mudela version ", last_conversion,	")\n";
}
  
  
 sub usage
  {
     print STDERR "Usage: convert-mudela [options] [mudela-file]...\n"
     . "Convert old mudela source from mudela-file or stdin\n\n"
     . "Options:\n"
     . "  -e, --edit             perform in-place conversion\n"
     . "  -f, --from=PATHLEVEL   use source version 0.0.PATCHLEVEL\n"
     . "  -h, --help             print this help\n"
     . "  -o, --output=FILE      name output file\n"
     . "  -s, --show-rules       print all known conversion rules\n"
     . "  -t, --to=VERSION       convert to version VERSION\n"
  }
      

my %minor_conversions = ("0.0.50" => \&no_conv,
 			 "0.0.52" => \&convert_0_0_50_to_0_0_52,
  			 "0.0.53" => \&convert_0_0_52_to_0_0_53,
			 "0.0.54" => \&convert_0_0_53_to_0_0_54,
			 "0.0.55" => \&convert_0_0_54_to_0_0_55,
			 "0.0.56" => \&convert_0_0_55_to_0_0_56,
			 "0.0.57" => \&convert_0_0_56_to_0_0_57,
			 "0.0.58" => \&convert_0_0_57_to_0_0_58,
			 "0.0.59" => \&convert_0_0_58_to_0_0_59,
			 "0.0.60" => \&convert_0_0_59_to_0_0_60,
			 "0.0.61" => \&convert_0_0_60_to_0_0_61,
			 "0.1.1" => \&convert_0_1_0_to_0_1_1,
			 "0.1.2" => \&no_conv,
			 "0.1.3" => \&convert_0_1_2_to_0_1_3,
			 "0.1.4" => \&no_conv,
			 "0.1.5" => \&convert_0_1_4_to_0_1_5,
			 "0.1.6" => \&convert_0_1_5_to_0_1_6
			 ,"0.1.7" => \&convert_0_1_6_to_0_1_7
			 ,"0.1.8" => \&convert_0_1_7_to_0_1_8
			 ,"0.1.9" => \&convert_0_1_8_to_0_1_9
			 ,"0.1.10" => \&convert_0_1_9_to_0_1_10
			 , "0.1.14" => \&convert_0_1_10_to_0_1_14
			 );

 

sub versions 
{
    return (sort { cmpver; } (keys %minor_conversions));
}


sub show_rules
{
    my (@v) = versions;

    print "Rules: ", join(", ", @v), "\n";
    
}

sub do_conversion
{
    my ($from,$to) = @_;

    my @applicable_conversion;
    my @mudela_levels;
    
    my @v = versions;
    foreach $ver (@v) {
	if (version_compare($ver, $from) > 0 && version_compare($ver,$to) <= 0 ){ 
	    push @applicable_conversion, $minor_conversions{$ver};
	    push @mudela_levels, $ver;
	}
    }
    
    print STDERR "Applying following rules: ", join(", ", @mudela_levels) , "\n";

    while (<INLY>) {
	generic_conversion_scan;
	foreach $subroutine (@applicable_conversion) {
	
	    &$subroutine;
	    
	}
	version_string_conv $from, $to;
	print OUTLY;
    }
}

sub get_auto_from
{
    my ($fn)=@_;
    my ($ver);
    open INLY, $fn || die "Can't open";

    while (<INLY>) {
	s/^.*\\version \"([^\"]*)\".*$//;
	if (defined ($1)) {
	    print STDERR "Guessing version: ", $1, ".. ";
	    $ver = $1;
	    last;
	}
    }
    if (!defined($ver)){
	print STDERR "can't determine mudela version in $fn.\n";
	my $u;
	return $u;
    }
    close INLY;
    return $ver;
}   

sub  set_files 
{
    $infile = "-";
    $outfile = "-";
    $outfile = $opt_output if (defined($opt_output));

    if ($ARGV [0])  {
	$infile = $ARGV[0];
    } 
    if (!(-f $infile) && !($infile =~ /\.ly$/s)) {
	$infile .= ".ly";
    }
    if ($opt_edit && $infile ne "-") {
	$opt_edit = 1;
	$outfile = "$infile.NEW";
	$infile = "$infile";
    }
    print STDERR "Input ", (($infile eq "-") ?"STDIN" : $infile), " .. ";

}

sub do_one_arg
{
    set_files;

    local ($from_version, $to_version);
    $from_version = $opt_from;
    $to_version = $opt_to;
    
    ($from_version = get_auto_from $infile) unless defined($opt_from);
    return if (!defined($from_version));
    
    ($to_version =  last_conversion) unless (defined($opt_to));

    die "can't open \`$infile\'" unless open INLY,$infile ;
    die "can't open \`$outfile\'" unless open OUTLY, ">$outfile";
    
    do_conversion $from_version, $to_version;
    close INLY;
    close OUTLY;

    if ($opt_edit) {
	rename $infile, "$infile~";
	rename $outfile, "$infile";
    }
}

## "main"

identify;


GetOptions ("help", "output=s", "from=s", "to=s", "minor=i", "edit", "show-rules");

if ($opt_help) {
    usage();
    $opt_help = 0;	# to extinguish typo check.
    exit 0;
}

if ($opt_show_rules) { 
    show_rules ;
    $opt_show_rules = 0;	# to extinguish typo check.
    exit 0;
}

local ( $infile,$outfile);
my $processed_one=0;

while (defined($ARGV[0])) {
    do_one_arg;
    shift @ARGV;
    $processed_one = 1;
}
do_one_arg unless ($processed_one);

    
