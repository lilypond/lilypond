#!@PERL@

open GREP, "egrep -h '^struct|^class' *.hh *.cc|";
open OUT, "|sort | uniq";
while (<GREP>) {

	s/^struct/class/;
	if (! /; *$/) {
		s/:[^{]+{.*$//;
		s/ *{.*$/;/;
	}
	if (! /; *$/) {
		chop;
		$_ .= ";\n";
		
	}
	print OUT;
}
close OUT;
close GREP;
