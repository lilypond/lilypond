#include <iostream.h>
#include <strstream.h>
#include "proto.hh"
#include "plist.hh"
#include "lexer.hh"
#include "debug.hh"
#include "main.hh"
#include "sourcefile.hh"
#include "source.hh"

My_flex_lexer *lexer=0;

int
yylex() {
	return lexer->yylex();
}

void
yyerror(const char *s)
{
	lexer->LexerError(s);
}

bool
busy_parsing()
{
    return lexer;	
}

Input_file::Input_file(String s)
{
    name = s;
    line = 1;
    String pf(s);
    if (pf=="") {
	is = &cin;
        sourcefile_l_ = 0;
    }
    else {
	Source_file* sourcefile_p = new Source_file( pf );
	source_l->add( sourcefile_p );
	sourcefile_l_ = sourcefile_p;
	is = sourcefile_l_->istream_l();
    }
    cout << "["<<pf<<flush;
}

Input_file::~Input_file()
{
  cout << "]" << flush;  
}
