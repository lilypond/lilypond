%{ // -*-Fundamental-*-
/*
  lexer.l -- implement the Flex lexer

  source file of the LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


/*
  backup rules

  after making a change to the lexer rules, run 
      flex -b <this lexer file>
  and make sure that 
      lex.backup
  contains no backup states, but only the reminder
      Compressed tables always back up.
  (don-t forget to rm lex.yy.cc :-)
 */


#include <stdio.h>

#include "string.hh"
#include "string-convert.hh"
#include "my-lily-lexer.hh"
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

#define yylval (*(YYSTYPE*)lexval_l)

#define YY_USER_ACTION	add_lexed_char(YYLeng());
%}

%option c++
%option noyywrap
%option nodefault
%option debug
%option yyclass="My_lily_lexer"
%option stack
%option never-interactive 
%option warn

%x incl
%x lyrics
%x notes
%x quote
%x longcomment


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
REAL		{INT}?(\.{N}+)?
KEYWORD		\\{WORD}
WHITE		[ \n\t\f]
BLACK		[^ \n\t\f]
RESTNAME	[rs]
NOTECOMMAND	\\{A}+
DOTS		\.+
LYRICS		({AA}|{NATIONAL})[^0-9 \t\n\f]*

%%


<notes,incl,INITIAL,lyrics>{
  "%{"	{
	yy_push_state(longcomment);
  }
  %[^{\n].*\n	{
  }
  %[^{\n]	{ // backup rule
  }
  %\n	{
  }
  %[^{\n].*	{
  }
  {WHITE}+ 	{
	
  }
}

<longcomment>{
	[^\%]* 		{
	}
	\%*[^}%]*		{

	}
	"%"+"}"		{
		yy_pop_state();
	}
}
<longcomment><<EOF>> {
	LexerError("EOF found inside a comment");
	if (! close_input()) { 
 	  yyterminate(); // can't move this, since it actually rets a YY_NULL
	}
}
<notes,INITIAL,lyrics>
\\include           {
	yy_push_state(incl);
}
<incl>\"[^"]*\"   { /* got the include file name */
	String s (YYText()+1);
	s = s.left_str(s.length_i()-1);
	mtor << "#include `" << s << "\'\n";
	new_input(s,source_l_g);
	yy_pop_state();
}
<incl>\"[^"]*   { // backup rule
	cerr << "missing end quote" << endl;
	exit( 1 );
}
<notes>{RESTNAME} 	{
	const char *s = YYText();
	yylval.string = new String (s);	
	mtor << "rest:"<< yylval.string;
	return RESTNAME;
}
<INITIAL,lyrics,notes>\\\${BLACK}*{WHITE}	{
	String s=YYText() + 2;
	s=s.left_str(s.length_i() - 1);
	return scan_escaped_word(s);
}
<INITIAL,lyrics,notes>\${BLACK}*{WHITE}		{
	String s=YYText() + 1;
	s=s.left_str(s.length_i() - 1);
	return scan_bare_word(s);
}
<INITIAL,lyrics,notes>\\\${BLACK}*		{ // backup rule
	cerr << "white expected" << endl;
	exit( 1 );
}
<INITIAL,lyrics,notes>\${BLACK}*		{ // backup rule
	cerr << "white expected" << endl;
	exit( 1 );
}
<notes>{
	{ALPHAWORD}/\'	{
		post_quotes_b_ = true;
		return scan_bare_word(YYText());
	}
	\'+		{
		yylval.i = YYLeng();
		if (post_quotes_b_) {
			post_quotes_b_ = false;
			return POST_QUOTES;
		} else
			return PRE_QUOTES;
	}
	{ALPHAWORD}	{
		return scan_bare_word(YYText());

	}

	{NOTECOMMAND}	{
		return scan_escaped_word(YYText()+1);
	}

	{DOTS}		{
		yylval.i = strlen(YYText());
		return DOTS;
	}
	{INT}		{
		yylval.i = String_convert::dec2_i( String( YYText() ) );
		return INT;
	}

	\+\+		{
		return CONCAT;
	}
	\" {
		start_quote();
	}
}

\"		{
	start_quote();
}
<quote>{
	\\\\	{
		*yylval.string += '\\';
	}
	\\\"	{
		*yylval.string +='\"';
	}
	[^"]+	{
		*yylval.string += YYText();
	}
	\"	{
		mtor << "quoted string: `" << *yylval.string << "'\n";
		yy_pop_state();
		return STRING;
	}
}

<lyrics>{

	\" {
		start_quote();
	}
	{DOTS}		{
		yylval.i = strlen(YYText());
		return DOTS;
	}
	{INT}		{
		yylval.i = String_convert::dec2_i( String( YYText() ) );
		return INT;
	}
	{NOTECOMMAND}	{
		return scan_escaped_word(YYText()+1);
	}
	{LYRICS} {
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
	. {
		return yylval.c = YYText()[0];
	}
}

<<EOF>> {
	mtor << "<<eof>>";

	if (! close_input()) { 
 	  yyterminate(); // can't move this, since it actually rets a YY_NULL
	}
}
{WORD}	{
	return scan_bare_word(YYText());
}
{KEYWORD}	{
	return scan_escaped_word(YYText()+1);
}
{REAL}		{
	Real r;
	int cnv=sscanf (YYText(), "%lf", &r);
	assert(cnv == 1);
	mtor  << "REAL" << r<<'\n';
	yylval.real = r;
	return REAL;
}

{INT}\. { // backup rule
	cerr << "int. encountered" << endl;
	exit( 1 );
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

<INITIAL,notes>.	{
	return yylval.c = YYText()[0];
}

<INITIAL,lyrics,notes>\\. {
    char c= YYText()[1];
    yylval.c = c;
    switch (c) {
    case '>':
	return E_BIGGER;
    case '<':
	return E_SMALLER;
    case '!':
	return E_EXCLAMATION;
    default:
	return E_CHAR;
    }
}

<*>.		{
	LexerError( String( "illegal character: " ) +String( YYText()[0] ));
	return YYText()[0];
}

%%

void
My_lily_lexer::push_note_state()
{
	yy_push_state(notes);
}

void
My_lily_lexer::push_lyric_state()
{
	yy_push_state(lyrics);
}
void
My_lily_lexer::pop_state()
{
	yy_pop_state();
}

int
My_lily_lexer::scan_escaped_word(String str)
{	
	mtor << "\\word: `" << str<<"'\n";
	int l = lookup_keyword(str);
	if (l != -1) {
		mtor << "(keyword)\n";
		return l;
	}
	Identifier * id = lookup_identifier(str);
	if (id) {
		mtor << "(identifier)\n";
		yylval.id = id;
		return id->token_code_i_;
	}
	mtor << "(string)";
	String *sp = new String( str);
	yylval.string=sp;
	return STRING;
}

int
My_lily_lexer::scan_bare_word(String str)
{
	mtor << "word: `" << str<< "'\n";	
	if (YYSTATE == notes){
		Melodic_req * mel_l = lookup_melodic_req_l(str);
		if (mel_l) {
		    mtor << "(notename)\n";
		    yylval.melreq = mel_l;
		    return NOTENAME_ID;
		}
	}
	if (YYSTATE != notes) {
		// ugr. Should do this in note mode?
		Identifier * id = lookup_identifier(str);
		if (id) {
			mtor << "(identifier)\n";
			yylval.id = id;
			return id->token_code_i_;
		}
	}
	yylval.string=new String( str );
	return STRING;
}

bool
My_lily_lexer::note_state_b() const
{
	return YY_START == notes;
}

bool
My_lily_lexer::lyric_state_b() const
{
	return YY_START == lyrics;
}
