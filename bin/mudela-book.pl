#!@PERL@ -w
# -*-Perl-*-
my $mudcount = 0;
my $mudela_b = 0;
my $outname = "-";
my $outdir;
use Getopt::Long;


# do something, check return status
sub my_system
{
    my (@cmds) = @_;
    foreach $cmd (@cmds) {
	my ($ignoreret)=0;
	if ( $cmd  =~ /^-/ ) {
	    $ignoreret = 1;
	    $cmd = substr ($cmd, 1);
	}
	
	my $ret =  ( system ($cmd));
	if ($ret) {
	    if ($ignoreret) {
		print STDERR "ignoring failed command \`$cmd\' (status $ret)\n";
	    }else {
		print STDERR "\nmudela-book: failed on command \`$cmd\' (status $ret)\n";
		exit 2;
	    }
	}
    }
}
sub gen_mufile
{
    return "$outdir/$outname$mudcount.ly";
}

sub gen_texbase
{
    return "$outname$mudcount.tex";
}
sub gen_texfile
{
    return "$outdir/" . gen_texbase;
}

sub close_mudela
{
    $mudela_b = 0;
    if ($fragment_b) {
	print MUDELA "}\n \\paper { linewidth = -1.0\\cm; castingalgorithm = \\Wordwrap; } }\n";
	$fragment_b =0;
    }
    if ( $verbatim_b)  {
	print BOOK "\\end{verbatim}\n\\interexample";
	$verbatim_b =0;
    }
    close MUDELA;
    my $status =0;
    if ( -f gen_mufile ) {
	$status = system "diff -q $outdir/book-mudela.ly " . gen_mufile;
    } else {
	$status = 1;
	}
    if ( $status ) {
	rename "$outdir/book-mudela.ly", gen_mufile;
	unlink gen_texfile;
    }
    
    if ( ! -f gen_texfile) {
	my_system "lilypond ". gen_mufile;
	rename gen_texbase, gen_texfile;
    }
    print BOOK "\\preexample\\input " . gen_texfile . "\n\\postexample\n";
	
}

sub open_mudela
{
    $mudcount++;
    $mudela_b = 1	;
    open MUDELA, ">$outdir/book-mudela.ly";
    if ($verbatim_b) {
	print BOOK "\\begin{verbatim}\n";
    }
    if ($fragment_b) {
	print MUDELA "\\score { \\melodic {\\octave c';";
    }

}

sub begin_b
{
    my ($s) = @_;
    return (/^\\begin{$s}/) ;    
}

sub end_b
{
    my ($s) = @_;
    return (/^\\end{$s}/) ;    
}
sub parse_mudela_opts
{
   my ($s) = @_;
   $s =~ s/[\[\]]//g;

   $verbatim_b =1 if ($s =~ /verbatim/ );
   $fragment_b = 1 if ($s =~ /fragment/ );
}   

sub help
{
    print  "usage: convert-mudela [options] [file]
options: 
--help
--outdir=DIRECTORY	write all files in directory DIRECTORY
--outname=NAME		use NAME as base  for the output
";
    exit;
}
    
sub main
{
    GetOptions( 'outdir=s', 'outname=s', 'help');
    help    if ( $opt_help ) ;

    if  (defined ($opt_outdir)) {
	$outdir = $opt_outdir .  "/";
    } else {
	$outdir = ".";
    }
   
    if (defined ($ARGV[0])) {
    	$infile = $ARGV[0] ;
    } else {
	$infile = "-";
    }
    if (defined ($opt_outname)) {
	$outname = $opt_outname ;
    } else { 
	die "Need to have an output name, use --outname" if ( $infile eq "-");
	$outname = "$infile.tex";
    }

    my $openout ="$outdir$outname"; 
    if  ( $infile eq $openout ) {
	die "The input can't be the output\n";
    }

    open INFILE, "<$infile";
    open BOOK, ">$openout";
    while (<INFILE>) {
	if ($mudela_b) {
	    if (end_b "mudela") {
		close_mudela;
		next;
	    }
	    print MUDELA;
	    if ( $verbatim_b ) {
		my $s = $_;
		$s =~ s/\t/    /g; #shit
		print BOOK $s;
	    }
	    
	} else {
	    if (/^\\begin(\[.*\])?{mudela}/ ) {
		my $opts ="";
		$opts = $1 if ( defined ($1));

		parse_mudela_opts($opts);
		open_mudela;
		next;  
	    } 
	    print BOOK;
	}
    }
    close INFILE;
    close BOOK;
}


main;
