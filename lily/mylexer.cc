#include <strstream.h>

#include "interval.hh"
#include "identparent.hh"
#include "assoc-iter.hh"
#include "lexer.hh"
#include "input-file.hh"
#include "out/parser.hh"
#include "keyword.hh"
#include "assoc.hh"
#include "lexer.hh"
#include "debug.hh"

#include "source-file.hh"
#include "parseconstruct.hh"

static Keyword_ent the_key_tab[]={
    "bar", BAR,
    "cadenza", CADENZA,
    "clef", CLEF,
    "cm", CM_T,
    "command", COMMAND,
    "commands", COMMANDS,    
    "duration", DURATIONCOMMAND,
    "dynamic", DYNAMIC,
    "geometric", GEOMETRIC,
    "goto", GOTO,
    "in", IN_T,
    "key", KEY,
    "melodic" , MELODIC,
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

My_flex_lexer::My_flex_lexer()
{
    keytable_p_ = new Keyword_table(the_key_tab);
    identifier_assoc_p_ = new Assoc<String, Identifier*>;
    errorlevel_i_ = 0;
}

int
My_flex_lexer::lookup_keyword(String s)
{
    return keytable_p_->lookup(s);
}

Identifier*
My_flex_lexer::lookup_identifier(String s)
{
    if (!identifier_assoc_p_->elt_query(s))
	return 0;
    
    return (*identifier_assoc_p_)[s];
}

char const*
My_flex_lexer::here_ch_c_l()
{
    return include_stack_.top()->sourcefile_l_->ch_c_l() + yyin->tellg();
}

void
My_flex_lexer::add_identifier(Identifier*i)
{
    delete lookup_identifier(i->name);
    (*identifier_assoc_p_)[i->name] = i;
}

My_flex_lexer::~My_flex_lexer()
{
    delete keytable_p_;

    for (Assoc_iter<String,Identifier*>
	     ai(*identifier_assoc_p_); ai.ok(); ai++) {
	mtor << "deleting: " << ai.key()<<'\n';
	delete ai.val();
    }
    delete identifier_assoc_p_;
}
void
My_flex_lexer::print_declarations()const
{
    for (Assoc_iter<String,Identifier*> ai(*identifier_assoc_p_); ai.ok(); ai++) {
	ai.val()->print();
    }
}

String
My_flex_lexer::spot()const
{
    return include_stack_.top()->name +  ": " + String( lineno() );
}

void
My_flex_lexer::LexerError(const char *s)
{
    if (lexer->include_stack_.empty()) {
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
   if (!include_stack_.empty()) {
	include_stack_.top()->line = lineno();
	     // should this be saved at all?
	include_stack_.top()->defined_ch_c_l_ = defined_ch_c_l;
   }

   Input_file *newin = new Input_file(s);
   include_stack_.push(newin);
   switch_streams(newin->is);

   yylineno = 1;
}

// pop the inputstack.
bool
My_flex_lexer::close_input()
{
    Input_file *old = include_stack_.pop();
     bool ok = 	true;
    if (include_stack_.empty()) {
	ok = false;
    } else {
	Input_file *i = include_stack_.top();
	switch_streams(i->is);
	yylineno = i->line;	
	defined_ch_c_l = i->defined_ch_c_l_;
    }
    delete old;
    return ok;
}
