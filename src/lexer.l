%{ // -*-Fundamental-*-

#include <stdio.h>

#include "string.hh"
#include "notename.hh"
#include "lexer.hh"
#include "varray.hh"
#include "parser.hh"
#include "debug.hh"

%}

%option c++
%option noyywrap
%option nodefault
%option yylineno
%option debug
%option yyclass="My_flex_lexer"
%x notes
%x incl
%x quote
%x lyrics



A		[a-zA-Z]
AA		{A}|_
N		[0-9]
AN		{AA}|{N}
PUNCT		[?!,.:;]
ACCENT		[\\'"^]
TEX		{AA}|-|{PUNCT}|{ACCENT}

WORD		{A}{AN}*
ALPHAWORD	{A}+
INT		-?{N}+
REAL		{INT}?(\.{N}*)?

OPTSIGN		!?
PITCHMOD	['`]*{OPTSIGN}
RESTNAME	r|s|p
NOTECOMMAND	\\{WORD}
NOTENAME	{ALPHAWORD}
DOTS		\.+
LYRICS		{TEX}+
COMMENT		[%#].*\n

%%

\$		{
	BEGIN(notes); 
}

\@		{
	BEGIN(lyrics); 
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
<notes>{COMMENT}	{
}
<notes>[ \t\n]+		{

}
<notes>\$	{
	BEGIN(INITIAL); 
}
<notes>[{}]	{
	return YYText()[0];
	
}
<notes>\"[^\"]*\" {
	String s (YYText()+1);
	s = s.left(s.len()-1);
	yylval.string = new String(s);
	return STRING;
}
<notes>.	{
	return yylval.c = YYText()[0];
}

\"		{
	BEGIN(quote);
}
<quote>[^\"]*	{
	yylval.string = new String (YYText());
}
<quote>\"	{
	mtor << "quoted string\n";
	BEGIN(INITIAL);
	return STRING;
}

<lyrics>{DOTS}		{
	yylval.i = strlen(YYText());
	return DOTS;
}
<lyrics>{INT}		{
	yylval.i = String(YYText()).value();
	return INT;
}
<lyrics>{NOTECOMMAND}	{
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
<lyrics>\"[^\"]*\" {
	String s (YYText()+1);
	s = s.left(s.len()-1);
	yylval.string = new String(s);
	return STRING;
}
<lyrics>{LYRICS} {
	String s (YYText()); 
	int i = 0;
	while ((i=s.pos("_")) != 0)// ugh. Whats this.
		*((char*)s.cptr() + i - 1) = ' ';
	if ((i=s.pos("\\,")) !=0)
		{
		*((char*)s.cptr() + i) = 'c';
		s = s.left(i+1) + " " + s.right(s.len()-i-1);
		}
	yylval.string = new String(s);
	return STRING;
}
<lyrics>\|	{
	return YYText()[0];
}
<lyrics>{COMMENT}		{ 

}
<lyrics>[ \t\n]+		{
}
<lyrics>@	{
	BEGIN(INITIAL); 
}

<<EOF>> {
	mtor << "<<EOF>>";

	if (! close_input())
 	  yyterminate(); // can't move this, since it actually rets a YY_NULL
}


include           { BEGIN(incl); }
<incl>[ \t]*      { /* eat the whitespace */ }
<incl>\"[^"]*\"+   { /* got the include file name */
   String s (YYText()+1);
	s = s.left(s.len()-1);
   new_input(s);
   BEGIN(INITIAL);
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
	error("lexer error: illegal character '"+String(YYText()[0])+
	  "' encountered");
	return YYText()[0];
}

%%

