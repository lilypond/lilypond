/*
  my-lily-lexer.cc -- implement My_lily_lexer

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <strstream.h>
#include <ctype.h>

#include "interval.hh"
#include "identifier.hh"
#include "assoc-iter.hh"
#include "out/parser.hh"
#include "keyword.hh"
#include "assoc.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "source-file.hh"
#include "parseconstruct.hh"

static Keyword_ent the_key_tab[]={
    "bar", BAR,
    "cadenza", CADENZA,
    "clef", CLEF,
    "cm", CM_T,
    "duration", DURATIONCOMMAND,
    "absdynamic", ABSDYNAMIC,
    "group", GROUP,
    "geometric", GEOMETRIC,
    "in", IN_T,
    "inputregister", INPUT_REGS,
    "lyric", LYRIC,
    "key", KEY,
    "melodic" , MELODIC,
    "melodic_request", MELODIC_REQUEST,
    "meter", METER,
    "midi", MIDI,
    "mm", MM_T,
    "multivoice", MULTIVOICE,
    "note", NOTE,
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
    "spandynamic", SPANDYNAMIC, 
    "symboltables", SYMBOLTABLES,
    "tempo", TEMPO,
    "texid", TEXID,
    "textstyle", TEXTSTYLE,
    "transpose", TRANSPOSE,
    "unitspace", UNITSPACE,
    "width", WIDTH,
    "grouping", GROUPING,
    0,0
};

My_lily_lexer::My_lily_lexer()
{
    keytable_p_ = new Keyword_table(the_key_tab);
    identifier_assoc_p_ = new Assoc<String, Identifier*>;
    errorlevel_i_ = 0;
    post_quotes_b_ = false;
    
}

int
My_lily_lexer::lookup_keyword(String s)
{
    return keytable_p_->lookup(s);
}

Identifier*
My_lily_lexer::lookup_identifier(String s)
{
    if (!identifier_assoc_p_->elt_query(s))
	return 0;
    
    return (*identifier_assoc_p_)[s];
}


void
My_lily_lexer::add_identifier(Identifier*i)
{
    delete lookup_identifier(i->name);
    (*identifier_assoc_p_)[i->name] = i;
}

My_lily_lexer::~My_lily_lexer()
{
    delete keytable_p_;

    for (Assoc_iter<String,Identifier*>
	     ai(*identifier_assoc_p_); ai.ok(); ai++) {
	mtor << "deleting: " << ai.key()<<'\n';
	Identifier *i_p = ai.val();
	if (!i_p->accessed_b_ && !i_p->init_b_)
	    i_p->warning("Variable not used");
	
	delete ai.val();
    }
    delete identifier_assoc_p_;
}
void
My_lily_lexer::print_declarations(bool init_b)const
{
    for (Assoc_iter<String,Identifier*> ai(*identifier_assoc_p_); ai.ok(); 
	 ai++) {
	if (ai.val()->init_b_ == init_b)
	    ai.val()->print();
    }
}

void
My_lily_lexer::LexerError(char const *s)
{
    if (include_stack_.empty()) {
	*mlog << "error at EOF" << s << '\n';
    } else {
	char const* ch_C = here_ch_C();
	if ( ch_C ) {
	    ch_C--;
	    while (isspace(*ch_C == ' ' ))
		    ch_C--;
	    ch_C++;
	}
	errorlevel_i_ |= 1;
	error( s, ch_C );
    }
}

