#include <fstream.h>
#include "lexer.hh"
#include "debug.hh"
#include "main.hh"

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
    if (pf=="")
	is = &cin;
    else {
	pf =find_file(pf);
	if (pf=="") {
	    String e("can\'t open `"  + s+"\'");
	    error(e);
	}
	is = new ifstream(  pf);
    }
    cout << "["<<pf<<flush;
}

Input_file::~Input_file()
{
  if (is != &cin)
      delete is;
  cout << "]" << flush;  
}
