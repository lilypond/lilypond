%{ // -*-Fundamental-*-

#include <fstream.h>
#include <stdio.h>
#include "glob.hh"
#include "string.hh"

#include "lexer.hh"
#include "keyword.hh"
#include "vray.hh"
#include "parser.hh"
#include "debug.hh"

sstack<istream *> include_stack;
static int last_print;
const int DOTPRINT=50; // every 50 lines dots
%}

%option c++
%option noyywrap
%option nodefault
%option yylineno
%option debug
%x notes
%x incl

OPTSIGN		!?
NOTENAMEI       A|B|C|D|E|F|G|As|Bes|Ces|Des|Es|Fes|Ges|Ais|Bis|Cis|Dis|Eis|Fis|Gis
NOTENAMEII	a|b|c|d|e|f|g|as|bes|ces|des|es|fes|ges|ais|bis|cis|dis|eis|fis|gis
NOTENAMEIII      Ases|Beses|Ceses|Deses|Eses|Feses|Geses|Aisis|Bisis|Cisis|Disis|Eisis|Fisis|Gisis
NOTENAMEIIII      ases|beses|ceses|deses|eses|feses|geses|aisis|bisis|cisis|disis|eisis|fisis|gisis
RESTNAME        r|s
NOTENAME        {NOTENAMEI}|{NOTENAMEII}|{NOTENAMEIII}|{NOTENAMEIIII}
PITCH		['`]*{OPTSIGN}{NOTENAME}
DURNAME		1|2|4|8|16|32
DURATION	{DURNAME}\.*
FULLNOTE	{PITCH}{DURATION}?
WORD		[a-zA-Z]+
REAL		[0-9]+(\.[0-9]*)?

%%

\$		{
	BEGIN(notes); return '$';
}

<notes>{RESTNAME} 	{
	const char *s = YYText();
	yylval.string = new String (s);
  	if (debug_flags & DEBUGTOKEN)
		mtor << "rest:"<< yylval.string;
	return RESTNAME;
}
<notes>{PITCH}	{
	const char *s = YYText();
	yylval.string = new String (s);
	if (debug_flags & DEBUGTOKEN)
		mtor << "pitch:"<< yylval.string;
	return PITCH;
}
<notes>{DURATION}	{
	yylval.string = new String (YYText());
	return DURATION;
}
<notes>[:space:]+		{
}
<notes>[ \t\n]+		{
}
<notes>%.*		{

}
<notes>\$	{
	BEGIN(INITIAL); return '$';
}
<notes>.	{
         cout << "SCANNER <notes> HOLE `" << YYText()<<'\''<<endl; 
         return YYText()[0];
}
<<EOF>> {
	if(!close_input())
		yyterminate();
}
{WORD}		{
	int l = lookup_keyword(YYText());
	if (l == -1){
            yylval.id = lookup_identifier(YYText());
	    return IDENTIFIER;
        } else
            return l;
}

{REAL}		{
	Real r;
	sscanf (YYText(), "%lf", &r);
	yylval.real = r;
	return REAL;
}

[\{\}\[\]\(\)]	{
    	if (debug_flags & DEBUGTOKEN)
	    cout << "parens\n";
	return YYText()[0];
}
[ \t\n]+	{
	
}
%.*		{
	//ignore
}
.		{
	error("lexer error: illegal character '"+String(YYText()[0])+
	  "' encountered");
	return YYText()[0];
}

%%

yyFlexLexer *lexer=0;

// set the  new input to s, remember old file.
void
new_input(String s)
{    
    istream *newin ;
    
    if (s=="")
	newin = &cin;
    else
	newin = new ifstream( s ); //
    
   if ( ! *newin)
      error("cant open "  + s);
   cout << "["<<s<<flush;
   
   include_stack.push(newin);

   if (!lexer) {
       lexer = new yyFlexLexer;
       lexer->set_debug( bool(debug_flags & DEBUGPARSER));
   }		
   
   lexer->switch_streams(newin);
}


// pop the inputstack.
bool
close_input()
{

  istream *closing= include_stack.pop();
  if (closing != &cin)
      delete closing;
  
  cout << "]" << flush;
  
  if (include_stack.empty())
      return false ;  
  else 
      lexer->switch_streams(include_stack.top());  
  return true;  
}

int
yylex() {
	return lexer->yylex();
}

void
yyerror(char *s)
{
  *mlog << "error in line " << lexer->lineno() <<  ": " << s << '\n';
  exit(1);
}


#if 0

<notes>{NOTENAME} 	{
	yylval.string = new String (YYText());
	return NOTENAME;
}

#endif