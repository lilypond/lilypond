#include <fstream.h>
#include "lexer.hh"
#include "debug.hh"

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

void
kill_lexer()
{
	delete lexer;
	lexer = 0;
}

void
set_lexer()
{
    if (!lexer) {
       lexer = new My_flex_lexer;
       lexer->set_debug( !monitor.silence("Lexer") && check_debug);
   }		
}

Input_file::Input_file(String s)
{
    name = s;
    line = 1;
    if (s=="")
	is = &cin;
    else
	is = new ifstream( s );
    
   if ( ! *is) {
	String e("cant open "  + s);
      error(e);
   }
   cout << "["<<s<<flush;
}

Input_file::~Input_file()
{
  if (is != &cin)
      delete is;
  cout << "]" << flush;  
}
