%{ // -*-Fundamental-*-

#include <stdio.h>

#include "string.hh"
#include "string-convert.hh"
#include "lexer.hh"
#include "varray.hh"
#include "parser.hh"
#include "debug.hh"
#include "input-score.hh"
#include "parseconstruct.hh"
#include "main.hh"
#include "identifier.hh"

#define start_quote()	\
	yy_push_state(quote);\
	yylval.string = new String


%}

%option c++
%option noyywrap
%option nodefault
%option yylineno
%option debug
%option yyclass="My_flex_lexer"
%option stack

%x notes
%x incl
%x quote
%x lyrics



A		[a-zA-Z]
AA		{A}|_
N		[0-9]
AN		{AA}|{N}
PUNCT		[?!,.:;']
ACCENT		\\[`'"^]
NATIONAL	[\241-\377]
TEX		{AA}|-|{PUNCT}|{ACCENT}|{NATIONAL}

WORD		{A}{AN}*
ALPHAWORD	{A}+
INT		-?{N}+
REAL		{INT}?(\.{N}*)?

RESTNAME	r|s|p
NOTECOMMAND	\\{WORD}
DOTS		\.+
LYRICS		{TEX}+
COMMENT		%.*\n

%%




include           {
	yy_push_state(incl);
}
<incl>[ \t]*      { /* eat the whitespace */ }
<incl>\"[^"]*\"+   { /* got the include file name */
   String s (YYText()+1);
   s = s.left_str(s.length_i()-1);
   defined_ch_c_l = here_ch_c_l() - String( YYText() ).length_i() - 1;
   new_input(s);
   yy_pop_state();
}

<notes>{RESTNAME} 	{
	const char *s = YYText();
	yylval.string = new String (s);	
	mtor << "rest:"<< yylval.string;
	return RESTNAME;
}
<notes,INITIAL>{ALPHAWORD}	{
	String str = YYText();
	mtor << "word: `" << str<< "'\n";
	Identifier * id = lookup_identifier(str);
	if (id) {		
		yylval.id = id;
		return id->token_code_i_;
	}

	yylval.string=new String( str );
	return STRING;
}
<notes,INITIAL>{NOTECOMMAND}	{
	String c = YYText() +1;
	mtor << "\\word: `" << YYText()+1<<"'\n";
	int l = lookup_keyword(c);
	if (l != -1)
		return l;
	
	String *sp = new String( c);
	yylval.string=sp;
	return STRING;
}

<notes>{DOTS}		{
	yylval.i = strlen(YYText());
	return DOTS;
}
<notes>{INT}		{
	yylval.i = String_convert::dec2_i( String( YYText() ) );
	return INT;
}
<notes>{COMMENT}	{
}
<notes>[ \t\n]+		{

}
<notes>\+\+		{
	return CONCAT;
}
<notes>\" {
	start_quote();
}
<notes>.	{
	return yylval.c = YYText()[0];
}


\"		{
	start_quote();
}
<quote>[^"]+	{
	*yylval.string += YYText();
}
<quote>\"	{
	mtor << "quoted string: `" << *yylval.string << "'\n";
	yy_pop_state();
	return STRING;
}

<lyrics>\" {
	start_quote();
}
<lyrics>{DOTS}		{
	yylval.i = strlen(YYText());
	return DOTS;
}
<lyrics>{INT}		{
	yylval.i = String_convert::dec2_i( String( YYText() ) );
	return INT;
}
<lyrics>{NOTECOMMAND}	{
	String c = YYText() +1;
	mtor << "\\word: `" << YYText()+1<<"'\n";
	int l = lookup_keyword(c);
	if (l != -1)
		return l;

/* let's try passing tex's typesetting macros like \ss \alpha \c */
	String* str_p = new String(YYText());
	yylval.string=str_p;
	mtor << "\\word: `" << *str_p << "'\n";
	return STRING;	

/* and skip identifiers...
	Identifier * id = lookup_identifier(c);
	if (id) {		
		yylval.id = id;
		return IDENTIFIER;
	}
	String *sp = new String( c);

	yylval.string=sp;

	return STRING;
*/
}
<lyrics>{LYRICS} {
	/* ugr. This sux. */
	String s (YYText()); 
	int i = 0;
	while ((i=s.index_i("_")) != -1) // change word binding "_" to " "
		*(s.ch_l() + i) = ' ';
	if ((i=s.index_i("\\,")) != -1)   // change "\," to TeX's "\c "
		{
		*(s.ch_l() + i + 1) = 'c';
		s = s.left_str(i+2) + " " + s.right_str(s.length_i()-i-2);
		}
	yylval.string = new String(s);
	mtor << "lyric : `" << s << "'\n";
	return STRING;
}
<lyrics>\|	{
	return YYText()[0];
}
<lyrics>{COMMENT}		{ 

}
<lyrics>[{}]	{
	return YYText()[0];
}
<lyrics>[()\[\]|/.^>_-] {
	return yylval.c = YYText()[0];
}
<lyrics>[ \t\n]+		{
}

<<EOF>> {
	mtor << "<<EOF>>";

	if (! close_input())
 	  yyterminate(); // can't move this, since it actually rets a YY_NULL
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
[*:=]		{
	char c = YYText()[0];
	mtor << "misc char" <<c<<"\n";
	return c;
}
[ \t\n]+	{
	
}

{COMMENT}		{
	//ignore
}
.		{
	error( String( "illegal character: " ) + String( YYText()[0] ), here_ch_c_l() );
	return YYText()[0];
}

%%

void
My_flex_lexer::push_note_state()
{
	yy_push_state(notes);
}

void
My_flex_lexer::push_lyric_state()
{
	yy_push_state(lyrics);
}
void
My_flex_lexer::pop_state()
{
	yy_pop_state();
}
