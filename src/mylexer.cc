#include <strstream.h>
#include "interval.hh"
#include "identparent.hh"
#include "associter.hh"
#include "lexer.hh"
#include "parser.hh"
#include "keyword.hh"
#include "assoc.hh"
#include "lexer.hh"
#include "debug.hh"
#include "notename.hh"
#include "sourcefile.hh"
#include "parseconstruct.hh"

static Keyword_ent the_key_tab[]={
    "bar", BAR,
    "cadenza", CADENZA,
    "clef", CLEF,
    "cm", CM_T,
    "command", COMMAND,
    "commands", COMMANDS,
    "duration", DURATIONCOMMAND,
    "geometric", GEOMETRIC,
    "goto", GOTO,
    "in", IN_T,
    "key", KEY,

    "meter", METER,
    "midi", MIDI,
    "mm", MM_T,
    "multivoice", MULTIVOICE,
    "octave", OCTAVECOMMAND,
    "output", OUTPUT,
    "partial", PARTIAL,
    "paper", PAPER,
    "plet", PLET,
    "pt", PT_T,
    "score", SCORE,
    "script", SCRIPT,
    "skip", SKIP,
    "staff", STAFF,
    "start", START_T,
    "stem", STEM,
    "table", TABLE,
    "symboltables", SYMBOLTABLES,
    "notenames", NOTENAMES,
    "tempo", TEMPO,
    "texid", TEXID,
    "textstyle", TEXTSTYLE,
    "unitspace", UNITSPACE,
    "voice", VOICE,
    "voices", VOICES,
    "width", WIDTH,
    "music", MUSIC,
    "grouping", GROUPING,
    0,0
};

int
My_flex_lexer::ret_notename(int *p, String text, int octave_mod)
{
    text.lower();
    char const* ch_c_l = here_ch_c_l();
    if ( ch_c_l ) {
	ch_c_l--;
	while ( ( *ch_c_l == ' ' ) || ( *ch_c_l == '\t' ) || ( *ch_c_l == '\n' ) )
	    ch_c_l--;
	ch_c_l++;
    }
	
    lookup_notename(p[0], p[1], text);
    p[2] = octave_mod;
    mtor << "notename: "<< text <<eol;
    if (p[0] < 0) {

	errorlevel_i_ |= 1;
	error( String( "notename does not exist: " ) + YYText(), ch_c_l );
	p[0] = p[1] = 0;
    }
    return NOTENAME;
}

My_flex_lexer::My_flex_lexer()
{
    keytable = new Keyword_table(the_key_tab);
    the_id_tab = new Assoc<String, Identifier*>;
    defaulttab = 0;
    errorlevel_i_ = 0;
}

int
My_flex_lexer::lookup_keyword(String s)
{
    return keytable->lookup(s);
}

Identifier*
My_flex_lexer::lookup_identifier(String s)
{
    if (!the_id_tab->elt_query(s))
	return 0;
    
    return (*the_id_tab)[s];
}

char const*
My_flex_lexer::here_ch_c_l()
{
    return include_stack.top()->sourcefile_l_->ch_c_l() + yyin->tellg();
}

void
My_flex_lexer::add_identifier(Identifier*i)
{
    delete lookup_identifier(i->name);
    (*the_id_tab)[i->name] = i;
}

My_flex_lexer::~My_flex_lexer()
{
    delete keytable;
    delete defaulttab;
    for (Assoc_iter<String,Identifier*> ai(*the_id_tab); ai.ok(); ai++) {
	mtor << "deleting: " << ai.key()<<'\n';
	delete ai.val();
    }
    delete the_id_tab;
}

String
My_flex_lexer::spot()const
{
    return include_stack.top()->name +  ": " + String( lineno() );
}

void
My_flex_lexer::LexerError(const char *s)
{
    if (lexer->include_stack.empty()) {
	*mlog << "error at EOF" << s << '\n';
    } else {
	char const* ch_c_l = here_ch_c_l();
	if ( ch_c_l ) {
	    ch_c_l--;
	    while ( ( *ch_c_l == ' ' ) || ( *ch_c_l == '\t' ) || ( *ch_c_l == '\n' ) )
		    ch_c_l--;
	    ch_c_l++;
	}
	errorlevel_i_ |= 1;
	error( s, ch_c_l );
    }
}

// set the  new input to s, remember old file.
void
My_flex_lexer::new_input(String s)
{    
   if (!include_stack.empty()) {
	include_stack.top()->line = lineno();
	     // should this be saved at all?
	include_stack.top()->defined_ch_c_l_ = defined_ch_c_l;
   }

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
	defined_ch_c_l = i->defined_ch_c_l_;
    }
    delete old;
    return ok;
}
