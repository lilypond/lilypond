%{ // -*-Fundamental-*-

#include <fstream.h>
#include <stdio.h>
#include "glob.hh"
#include "string.hh"
#include "notename.hh"
#include "lexer.hh"
#include "keyword.hh"
#include "vray.hh"
#include "parser.hh"
#include "debug.hh"
#include "sstack.hh"

struct Input_file {
	istream*is;
	int line;
	String name;

	Input_file(String);
	~Input_file();
};

/// lexer with provisions for include files.
struct My_flex_lexer : yyFlexLexer {
    sstack<Input_file*> include_stack;
    void new_input(String s);
    bool  close_input();
};

My_flex_lexer *lexer=0;

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
%x quote



A		[a-zA-Z]
AA		{A}|_
N		[0-9]
AN		{AA}|{N}

WORD		{A}{AN}*
ALPHAWORD	{A}+
INT		-?{N}+
REAL		{INT}(\.{N}*)?

OPTSIGN		!?
PITCHMOD	['`]*{OPTSIGN}
RESTNAME	r|s|p
NOTECOMMAND	\\{WORD}
NOTENAME	{ALPHAWORD}
DOTS		\.+

%%

\$		{
	BEGIN(notes); 
}

<notes>{RESTNAME} 	{
	const char *s = YYText();
	yylval.string = new String (s);	
	mtor << "rest:"<< yylval.string;
	return RESTNAME;
}

<notes>{NOTENAME}	{
	int *p=yylval.ii;
	lookup_notename(p[0], p[1], YYText());
	mtor << "notename: "<< YYText()<<eol;
	if (p[0] < 0) {
		String e("notename does not exist: ");
		error(e + YYText());
	}
	return NOTENAME;
}

<notes>{NOTECOMMAND}	{
	String c = YYText() +1;
	mtor << "\\word: " << YYText()+1<<eol;
	int l = lookup_keyword(c);
	if (l != -1)
		return l;
	Identifier * id = lookup_identifier(c);
	if (id) {		
		yylval.id = id;
		return IDENTIFIER;
	}
	String *sp = new String( c);

	yylval.string=sp;
	return NEWIDENTIFIER;
}

<notes>{PITCHMOD}	{
	const char *s = YYText();
	mtor << "pitchmod:"<< YYText()<<eol;
	yylval.string = new String (s);
	return PITCHMOD;
}
<notes>{DOTS}		{
	yylval.i = strlen(YYText());
	return DOTS;
}
<notes>{INT}		{
	yylval.i = String(YYText()).value();
	return INT;
}
<notes>\|		{
}
<notes>[:space:]+		{
}
<notes>[%#].*\n		{ 

}
<notes>[ \t\n]+		{

}
<notes>\$	{
	BEGIN(INITIAL); 
}
<notes>[{}]	{
	return YYText()[0];
	
}
<notes>[\[)]	{ /* parens () are NO mistake */
	yylval.c = YYText()[0];
	return OPEN_REQUEST_PARENS;
}
<notes>[\](]	{ /* parens () are NO mistake */
	yylval.c = YYText()[0];
	return CLOSE_REQUEST_PARENS;
}

<notes>.	{
	String s("lexer error: illegal character found: " + String(YYText()));
	yyerror(s);
}

\"		{
	BEGIN(quote);
}
<quote>[^\"]*	{
	yylval.string = new String (YYText());
}
<quote>\"	{
	BEGIN(INITIAL);
	return STRING;
}

<<EOF>> {
	mtor << "<<EOF>>";

	if (! ((My_flex_lexer*) this)->close_input())
 	  yyterminate(); // can't move this, since it actually rets a YY_NULL
}

{WORD}		{
	mtor << "word: " << YYText()<<eol;
	String c = YYText();
	int l = lookup_keyword(c);
	if (l != -1)
		return l;
	Identifier * id = lookup_identifier(c);
	if (id) {		
		yylval.id = id;
		return IDENTIFIER;
	}
	String *sp = new String( c);
	mtor << "new id: " << *sp << eol;
	yylval.string=sp;
	return NEWIDENTIFIER;
}

{REAL}		{
	Real r;
	int cnv=sscanf (YYText(), "%lf", &r);
	assert(cnv == 1);
	mtor  << "REAL" << r<<'\n';
	yylval.real = r;
	return REAL;
}

[{}]	{

	mtor << "parens\n";
	return YYText()[0];
}
[:=]		{
	char c = YYText()[0];
	mtor << "misc char" <<c<<"\n";
	return c;
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

int
yylex() {
	return lexer->yylex();
}

void
yyerror(const char *s)
{
    String e;
    if (lexer->include_stack.empty()) {
	*mlog << "error at EOF" << s;
    }else 
	*mlog << lexer->include_stack.top()->name <<  ": " <<
	 lexer->lineno() <<  ": error:" << s << '\n';
  exit(1);
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
new_input(String s)
{
    if (!lexer) {
       lexer = new My_flex_lexer;
       lexer->set_debug( !monitor.silence("Lexer") && check_debug);
   }		
   lexer->new_input(s);
}

/****************/

Input_file::Input_file(String s)
{
    name = s;
    line = 1;
    if (s=="")
	is = &cin;
    else
	is = new ifstream( s ); //
    
   if ( ! *is) {
	String e("cant open "  + s);
	if (lexer)
	      yyerror(e);
	else
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
/****************/

// set the  new input to s, remember old file.
void
My_flex_lexer::new_input(String s)
{    
   if (!include_stack.empty())
	include_stack.top()->line = lineno();

   Input_file *newin = new Input_file(s);
   include_stack.push(newin);
   switch_streams(newin->is);
   yylineno = 1;
}

// pop the inputstack.
bool
My_flex_lexer::close_input()
{
    Input_file *old = include_stack.pop();
     bool ok = 	true;
    if (include_stack.empty()) {
	ok = false;
    } else {
	Input_file *i = include_stack.top();
	switch_streams(i->is);
	yylineno = i->line;	
    }
    delete old;
    return ok;
}
