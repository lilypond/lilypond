%{ // -*-Fundamental-*-

/*
  parser.yy -- Bison/C++ parser for LilyPond

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*

TODO:

* The rules for who is protecting what are very shady. Uniformise
  this.

* There are too many lexical modes?

*/

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>


#include "book.hh"

#include "context-def.hh"
#include "dimensions.hh"
#include "event.hh"
#include "file-path.hh"
#include "input-smob.hh"
#include "input.hh"
#include "lily-guile.hh"
#include "lilypond-input-version.hh"
#include "ly-module.hh"
#include "main.hh"
#include "misc.hh"
#include "music-list.hh"
#include "music-sequence.hh"
#include "lily-lexer.hh"
#include "lily-parser.hh"
#include "paper-book.hh"
#include "output-def.hh"
#include "scm-hash.hh"
#include "scm-option.hh"
#include "score.hh"
#include "text-item.hh"
#include "warn.hh"

#define MY_MAKE_MUSIC(x)  make_music_by_name (ly_symbol2scm (x))

Music *property_op_to_music (SCM op);
Music *context_spec_music (SCM type, SCM id, Music *m, SCM ops);
SCM get_next_unique_context ();

#define YYERROR_VERBOSE 1

#define YYPARSE_PARAM my_lily_parser
#define YYLEX_PARAM my_lily_parser
#define THIS\
	((Lily_parser *) my_lily_parser)

#define yyerror THIS->parser_error

/* Add symbols to the TAGS field of a music object.  */

void
tag_music (Music *m, SCM tag, Input ip)
{
	SCM tags = m->get_property ("tags");
	if (scm_is_symbol (tag))
		tags = scm_cons (tag, tags);
	else if (ly_c_list_p (tag))
		tags = ly_append2 (tag, tags);
	else
		ip.warning (_ ("Tag must be symbol or list of symbols."));

	m->set_property ("tags", tags);
}

bool
is_regular_identifier (SCM id)
{
  String str = ly_scm2string (id);
  char const *s = str.to_str0 ();

  bool v = true;
#if 0
  isalpha (*s);
  s++;
#endif
  while (*s && v)
   {
        v = v && isalnum (*s);
        s++;
   }
  return v;
}


SCM
get_first_context_id (SCM type, Music *m)
{
	SCM id = m->get_property ("context-id");
	if (SCM_BOOL_T == scm_equal_p (m->get_property ("context-type"), type)
	    && scm_is_string (m->get_property ("context-id"))
	    && scm_c_string_length (id) > 0)
	{
		return id;
	}
	return SCM_EOL;
}

SCM
make_simple_markup (SCM encoding, SCM a)
{
	SCM simple = ly_scheme_function ("simple-markup");
	if (scm_is_symbol (encoding))
		return scm_list_3 (ly_scheme_function ("encoded-simple-markup"),
			   encoding, a);
	return scm_list_2 (simple, a);
}

bool
is_duration (int t)
{
  return t && t == 1 << intlog2 (t);
}

void
set_music_properties (Music *p, SCM a)
{
  for (SCM k = a; scm_is_pair (k); k = scm_cdr (k))
 	p->internal_set_property (scm_caar (k), scm_cdar (k));
}

SCM
make_chord_step (int step, int alter)
{
	if (step == 7)
		alter += FLAT;

	while (step < 0)
		step += 7;
	Pitch m ((step -1) / 7, (step - 1) % 7, alter);
	return m.smobbed_copy ();
}


SCM
make_chord (SCM pitch, SCM dur, SCM modification_list)
{
	SCM chord_ctor = ly_scheme_function ("construct-chord");
	SCM ch = scm_call_3 (chord_ctor, pitch, dur, modification_list);
	scm_gc_protect_object (ch);
	return ch;
}

/* Todo: actually also use apply iso. call too ...  */
bool
ly_input_procedure_p (SCM x)
{
	return ly_c_procedure_p (x)
		|| (scm_is_pair (x) && ly_c_procedure_p (scm_car (x)));
}

Music*
set_property_music (SCM sym, SCM value)
{
	Music *p = MY_MAKE_MUSIC ("PropertySet");
	p->set_property ("symbol", sym);
	p->set_property ("value", value);
	return p;
}

Music*
make_music_relative (Pitch start, Music *music)
{
	Music *relative = MY_MAKE_MUSIC ("RelativeOctaveMusic");
 	relative->set_property ("element", music->self_scm ());
	
 	Pitch last = music->to_relative_octave (start);
 	if (lily_1_8_relative)
 		music->set_property ("last-pitch", last.smobbed_copy ());
	return relative;
}

Music*
make_lyric_combine_music (SCM name, Music *music)
{
	Music *combine = MY_MAKE_MUSIC ("NewLyricCombineMusic");
	combine->set_property ("element", music->self_scm ());
 	combine->set_property ("associated-context", name);
	return combine;
}

%}

/* We use SCMs to do strings, because it saves us the trouble of
deleting them.  Let's hope that a stack overflow doesnt trigger a move
of the parse stack onto the heap. */

%left ADDLYRICS

%union {
	Book *book;
	Output_def *outputdef;
	SCM scm;
	String *string;
 	Music *music;
 	Score *score;
 	int i;
}
%{

int
yylex (YYSTYPE *s, void *v)
{
	Lily_parser *pars = (Lily_parser*) v;
	Lily_lexer *lex = pars->lexer_;

	lex->lexval = (void*) s;
	lex->prepare_for_next_token ();
	return lex->yylex ();
}


%}

%expect 3

/*
  Three shift/reduce problems:

2. \markup identifier.

  (what? --hwn)

3. \markup { }

  (what? --hwn)


4.  \repeat
	\repeat .. \alternative


    \repeat { \repeat .. \alternative }

or

    \repeat { \repeat } \alternative 

)
 */


%pure_parser

%token ACCEPTS
%token ADDQUOTE
%token LYRICSTO
%token ALIAS
%token ALTERNATIVE
%token BAR
%token BOOK
%token CHANGE
%token CHORDMODIFIERS
%token CHORDS
%token CHORDMODE
%token DOUBLE_ANGLE_OPEN
%token DOUBLE_ANGLE_CLOSE
%token CLEF
%token COMMANDSPANREQUEST
%token CONSISTS
%token CONTEXT
%token DEFAULT
%token DENIES
%token DESCRIPTION
%token EXTENDER
%token FIGURES
%token FIGUREMODE
%token FIGURE_OPEN FIGURE_CLOSE
%token FIGURE_BRACKET_CLOSE FIGURE_BRACKET_OPEN
%token GROBDESCRIPTIONS
%token HEADER
%token HYPHEN
%token INVALID
%token KEY
%token LAYOUT
%token LYRICS
%token LYRICMODE
%token MARK
%token MIDI
%token MULTI_MEASURE_REST
%token NAME
%token NEWCONTEXT
%token NOTEMODE
%token OCTAVE
%token ONCE
%token OVERRIDE SET REVERT
%token PAPER
%token PARTIAL
%token QUOTE
%token RELATIVE
%token REMOVE
%token REPEAT
%token REST
%token SCM_T
%token SCORE
%token SEQUENTIAL
%token ADDLYRICS
%token SIMULTANEOUS
%token SKIP
%token SPANREQUEST
%token TAG
%token TEMPO
%token TIMES
%token TIME_T
%token TRANSPOSE
%token TRANSPOSITION
%token TYPE
%token UNSET
%token WITH
%token MARKUP

/* escaped */
/* FIXME: this sucks.  The user will get to see these names:
    syntax error, unexpected E_CHAR:
		\
                 %%
  */

%token E_CHAR E_EXCLAMATION E_SMALLER E_BIGGER E_OPEN E_CLOSE
%token E_LEFTSQUARE E_RIGHTSQUARE E_TILDE
%token E_BACKSLASH
%token CHORD_BASS CHORD_COLON CHORD_MINUS CHORD_CARET  CHORD_SLASH
%token FIGURE_SPACE

%token <i>	DIGIT
%token <i>	UNSIGNED
%token <i> E_UNSIGNED
%token <id>	IDENTIFIER
%token <scm>	CHORDMODIFIER_PITCH
%token <scm>	DRUM_PITCH
%token <scm>	DURATION_IDENTIFIER
%token <scm>	EVENT_IDENTIFIER
%token <scm>	MUSIC_IDENTIFIER CONTEXT_DEF_IDENTIFIER
%token <scm>	NOTENAME_PITCH
%token <scm>	NUMBER_IDENTIFIER
%token <scm>	OUTPUT_DEF_IDENTIFIER
%token <scm>	RESTNAME
%token <scm> LYRICS_STRING
%token <scm>	SCM_T
%token <scm>	SCORE_IDENTIFIER
%token <scm>	STRING
%token <scm>	STRING_IDENTIFIER SCM_IDENTIFIER
%token <scm>	TONICNAME_PITCH
%token <scm> 	CHORD_MODIFIER
%token <scm>    FRACTION
%token <scm>   REAL
%token <scm> MARKUP_HEAD_EMPTY
%token <scm> MARKUP_HEAD_MARKUP0
%token <scm> MARKUP_HEAD_MARKUP0_MARKUP1
%token <scm> MARKUP_HEAD_SCM0
%token <scm> MARKUP_HEAD_SCM0_MARKUP1
%token <scm> MARKUP_HEAD_SCM0_SCM1
%token <scm> MARKUP_HEAD_SCM0_SCM1_MARKUP2
%token <scm> MARKUP_HEAD_SCM0_SCM1_SCM2
%token <scm> MARKUP_IDENTIFIER MARKUP_HEAD_LIST0
%token <scm> MUSIC_FUNCTION
%token <scm> MUSIC_FUNCTION_MUSIC 
%token <scm> MUSIC_FUNCTION_MUSIC_MUSIC 
%token <scm> MUSIC_FUNCTION_SCM 
%token <scm> MUSIC_FUNCTION_SCM_SCM 
%token <scm> MUSIC_FUNCTION_SCM_MUSIC 
%token <scm> MUSIC_FUNCTION_SCM_MUSIC_MUSIC 
%token <scm> MUSIC_FUNCTION_SCM_SCM_MUSIC 
%token <scm> MUSIC_FUNCTION_MARKUP 
%token <scm> MUSIC_FUNCTION_MARKUP_MARKUP 
%token <scm> MUSIC_FUNCTION_MARKUP_MUSIC 
%token <scm> MUSIC_FUNCTION_MARKUP_MUSIC_MUSIC 
%token <scm> MUSIC_FUNCTION_MARKUP_MARKUP_MUSIC 

%token DRUMS
%token DRUMMODE


%type <book>	book_block book_body
%type <i>	bare_int  bare_unsigned
%type <i>	exclamations questions dots optional_rest
%type <i>	script_dir
%type <i>	sub_quotes sup_quotes
%type <i>	tremolo_type
%type <i>  	bass_mod

%type <music>	Composite_music Simple_music Prefix_composite_music Generic_prefix_music
%type <music>	Grouped_music_list
%type <music>	Music Sequential_music Simultaneous_music
%type <music>	Repeated_music
%type <music>	command_req
%type <music>	gen_text_def direction_less_event direction_reqd_event
%type <music>	music_property_def context_change
%type <music>	note_chord_element chord_body chord_body_element
%type <music>	post_event tagged_post_event
%type <music>	relative_music re_rhythmed_music
%type <music>	simple_element event_chord command_element
%type <music>	string_number_event
%type <music>	toplevel_music
%type <music> 	tempo_event

%type <outputdef>	output_def_body output_def_head
%type <outputdef> output_def paper_block 

%type <scm>	Music_list
%type <scm>	chord_body_elements
%type <scm>	chord_item chord_items chord_separator new_chord
%type <scm>	context_def_spec_block context_def_spec_body
%type <scm>	context_mod context_def_mod optional_context_mod
%type <scm>	context_prop_spec
%type <scm>	direction_less_char
%type <scm>	duration_length fraction
%type <scm>	embedded_scm scalar
%type <scm>	identifier_init
%type <scm>	lilypond_header lilypond_header_body
%type <scm>	new_lyrics
%type <scm>	post_events
%type <scm>	property_operation
%type <scm>	script_abbreviation
%type <scm>	simple_string
%type <scm>	steno_pitch pitch absolute_pitch pitch_also_in_chords
%type <scm>	steno_tonic_pitch
%type <scm>	step_number step_numbers 
%type <scm>	string bare_number number_expression number_term number_factor
%type <scm> 	bass_number br_bass_figure bass_figure figure_list figure_spec
%type <scm> 	context_mod_list
%type <scm> 	octave_check
%type <scm> 	steno_duration optional_notemode_duration multiplied_duration
%type <scm>  	Generic_prefix_music_scm 
%type <scm>  	lyric_element
%type <scm>     Alternative_music
%type <scm>	markup markup_line markup_list markup_list_body full_markup
%type <scm> 	mode_changing_head
%type <scm> 	mode_changing_head_with_context

%type <score>	score_block score_body


%left '-' '+'

/* We don't assign precedence to / and *, because we might need varied
prec levels in different prods */

%left UNARY_MINUS

%%

lilypond:	/* empty */
	| lilypond toplevel_expression {
	}
	| lilypond assignment {
	}
	| lilypond error {
		THIS->error_level_ = 1;
	}
	| lilypond INVALID	{
		THIS->error_level_ = 1;
	}
	;

toplevel_expression:
	lilypond_header {
		THIS->lexer_->set_identifier (ly_symbol2scm ("$globalheader"), $1);
	}
	| add_quote {
	
	}
	| book_block {
		Book *book = $1;
		SCM proc = THIS->lexer_->lookup_identifier ("toplevel-book-handler");
		scm_call_2 (proc, THIS->self_scm (), book->self_scm ());
 		scm_gc_unprotect_object (book->self_scm ());
	}
	| score_block {
		Score *score = $1;
		
		SCM proc = THIS->lexer_->lookup_identifier ("toplevel-score-handler");
		scm_call_2 (proc, THIS->self_scm (), score->self_scm ());
 		scm_gc_unprotect_object (score->self_scm ());
	}
	| toplevel_music {
		Music *music = $1;
		SCM proc = THIS->lexer_->lookup_identifier ("toplevel-music-handler");
		scm_call_2 (proc, THIS->self_scm (), music->self_scm ());
 		scm_gc_unprotect_object (music->self_scm ());
	}
	| output_def {
		SCM id = SCM_EOL;
		Output_def * od = $1;

		if ($1->c_variable ("is-paper") == SCM_BOOL_T)
			id = ly_symbol2scm ("$defaultpaper");
		else if ($1->c_variable ("is-midi") == SCM_BOOL_T)
			id = ly_symbol2scm ("$defaultmidi");
		else if ($1->c_variable ("is-layout") == SCM_BOOL_T)
			id = ly_symbol2scm ("$defaultlayout");

		THIS->lexer_->set_identifier (id, od->self_scm ());
		scm_gc_unprotect_object (od->self_scm ());
	}
	;

toplevel_music:
	Composite_music {
	}
	;

embedded_scm:
	SCM_T
	| SCM_IDENTIFIER
	;


lilypond_header_body:
	{
		$$ = ly_make_anonymous_module (safe_global_b);
		THIS->lexer_->add_scope ($$);
	}
	| lilypond_header_body assignment  {
		
	}
	;

lilypond_header:
	HEADER '{' lilypond_header_body '}'	{
		$$ = THIS->lexer_->remove_scope ();
	}
	;


/*
	DECLARATIONS
*/
assignment:
	STRING {
		THIS->push_spot ();
	}
	/* cont */ '=' identifier_init  {

	/*
		Should find generic way of associating input with objects.
	*/
		Input ip = THIS->pop_spot ();

		if (! is_regular_identifier ($1))
		{
			ip.warning (_ ("Identifier should have alphabetic characters only"));
		}

	        THIS->lexer_->set_identifier ($1, $4);

/*
 TODO: devise standard for protection in parser.

  The parser stack lives on the C-stack, which means that
all objects can be unprotected as soon as they're here.

*/
	}
	| embedded_scm { }
	;


identifier_init:
	score_block {
		$$ = $1->self_scm ();
		scm_gc_unprotect_object ($$);
	}
	| output_def {
		$$ = $1->self_scm ();
		scm_gc_unprotect_object ($$);
	}
	| context_def_spec_block {
		$$ = $1;
	}
	| Music  {
		$$ = $1->self_scm ();
		scm_gc_unprotect_object ($$);
	}
	| post_event {
		$$ = $1->self_scm ();
		scm_gc_unprotect_object ($$);
	}
	| number_expression {
 		$$ = $1;
	}
	| string {
		$$ = $1;
	}
        | embedded_scm {
		$$ = $1;
	}
	| full_markup {
		$$ = $1;
	}
	| DIGIT {
		$$ = scm_int2num ($1);
	}
	;

context_def_spec_block:
	CONTEXT '{' context_def_spec_body '}'
		{
		$$ = $3;
	}
	;

context_def_spec_body:
	/**/ {
		$$ = Context_def::make_scm ();
		unsmob_context_def ($$)->set_spot (THIS->here_input ());
	}
	| CONTEXT_DEF_IDENTIFIER {
		$$ = $1;
		unsmob_context_def ($$)->set_spot (THIS->here_input ());
	}
	| context_def_spec_body GROBDESCRIPTIONS embedded_scm {
		Context_def*td = unsmob_context_def ($$);

		for (SCM p = $3; scm_is_pair (p); p = scm_cdr (p)) {
			SCM tag = scm_caar (p);

			/* TODO: should make new tag "grob-definition" ? */
			td->add_context_mod (scm_list_3 (ly_symbol2scm ("assign"),
							tag, scm_cons (scm_cdar (p), SCM_EOL)));
		}
	}
	| context_def_spec_body context_mod {
		unsmob_context_def ($$)->add_context_mod ($2);		
	}
	;



book_block:
	BOOK {
		THIS->push_spot ();
	}
	/*cont*/ '{' book_body '}' 	{
		THIS->pop_spot ();
		$$ = $4;
	}
	;

/* FIXME:
   * Use 'handlers' like for toplevel-* stuff?
   * grok \layout and \midi?  */
book_body:
	{
		$$ = new Book;
		$$->set_spot (THIS->here_input ());
		$$->paper_ = dynamic_cast<Output_def*> (unsmob_output_def (THIS->lexer_->lookup_identifier ("$defaultpaper"))->clone ());
		scm_gc_unprotect_object ($$->paper_->self_scm ());
		$$->header_ = THIS->lexer_->lookup_identifier ("$globalheader"); 
	}
	| book_body paper_block {
		$$->paper_ = $2;
		scm_gc_unprotect_object ($2->self_scm ());
	}
	| book_body score_block {
		Score *score = $2;
		$$->scores_.push (score);
		scm_gc_unprotect_object (score->self_scm ());
	}
	| book_body lilypond_header {
		$$->header_ = $2;
	}
	| book_body error {
		$$->paper_ = 0;
		$$->scores_.clear();
	}
	;

score_block:
	SCORE {
		THIS->push_spot ();
	}
	/*cont*/ '{' score_body '}' 	{
		THIS->pop_spot ();
		$$ = $4;
	}
	;

score_body:
	/**/	{
		$$ = new Score;
	
		$$->set_spot (THIS->here_input ());
	}
	| SCORE_IDENTIFIER {
		$$ = new Score ( *unsmob_score ($1));
		$$->set_spot (THIS->here_input ());
	}
	| score_body Music {
		SCM m = $2->self_scm ();
		scm_gc_unprotect_object (m);
		$$->set_music (m, THIS->self_scm ());
	}
	| score_body lilypond_header 	{
		$$->header_ = $2;
	}
	| score_body output_def {
		if ($2->lookup_variable (ly_symbol2scm ("is-paper")) == SCM_BOOL_T)
		{
			THIS->parser_error (_("\\paper cannot be in \\score. Use \\layout instead"));
		
		}
		else
		{
			$$->defs_.push ($2);
		}
		scm_gc_unprotect_object ($2->self_scm ());
	}
	| score_body error {
		$$->error_found_ = true;
	}
	;


/*
	OUTPUT DEF
*/

paper_block:
	output_def {
		$$ = $1;
		if ($$->lookup_variable (ly_symbol2scm ("is-paper")) != SCM_BOOL_T)
		{
			THIS->parser_error (_("Need \\paper for paper block."));
			$$ = get_paper (THIS);
		}
	}
	;


output_def:
	output_def_body '}' {
		$$ = $1;

		THIS->lexer_->remove_scope ();
		THIS->lexer_->pop_state ();
	}
	;

output_def_head:
	PAPER {
		$$ = get_paper (THIS);
		$$->input_origin_ = THIS->here_input ();
		THIS->lexer_->add_scope ($$->scope_);
	}
	| MIDI    {
		Output_def *p = get_midi (THIS);
		$$ = p;
		THIS->lexer_->add_scope (p->scope_);
	}
	| LAYOUT 	{
		Output_def *p = get_layout (THIS);

		THIS->lexer_->add_scope (p->scope_);
		$$ = p;
	}
	;


output_def_body:
	output_def_head '{' {
		$$ = $1;
		$$->input_origin_.set_spot (THIS->here_input ());
		THIS->lexer_->push_initial_state ();
	}
	| output_def_head '{' OUTPUT_DEF_IDENTIFIER 	{
		scm_gc_unprotect_object ($1->self_scm ());
		Output_def *o = unsmob_output_def ($3);
		o->input_origin_.set_spot (THIS->here_input ());
		$$ = o;
		THIS->lexer_->remove_scope ();
		THIS->lexer_->add_scope (o->scope_);
		THIS->lexer_->push_initial_state ();
	}
	| output_def_body assignment  {

	}
	| output_def_body context_def_spec_block	{
		assign_context_def ($$, $2);
	}
	| output_def_body tempo_event  {
		/*
			junk this ? there already is tempo stuff in
			music.
		*/
		int m = scm_to_int ($2->get_property ("metronome-count"));
		Duration *d = unsmob_duration ($2->get_property ("tempo-unit"));
		set_tempo ($$, d->get_length (), m);
		scm_gc_unprotect_object ($2->self_scm ());
	}
	| output_def_body error {

	}
	;

tempo_event:
	TEMPO steno_duration '=' bare_unsigned	{
		$$ = MY_MAKE_MUSIC ("MetronomeChangeEvent");
		$$->set_property ("tempo-unit", $2);
		$$->set_property ("metronome-count", scm_int2num ( $4));
	}
	;

/*
The representation of a  list is the

  (LIST . LAST-CONS)

 to have efficient append.  */

Music_list:
	/* empty */ {
		$$ = scm_cons (SCM_EOL, SCM_EOL);
	}
	| Music_list Music {
		SCM s = $$;
 		SCM c = scm_cons ($2->self_scm (), SCM_EOL);
		scm_gc_unprotect_object ($2->self_scm ()); /* UGH */

		if (scm_is_pair (scm_cdr (s)))
			scm_set_cdr_x (scm_cdr (s), c); /* append */
		else
			scm_set_car_x (s, c); /* set first cons */
		scm_set_cdr_x (s, c);  /* remember last cell */
	}
	| Music_list embedded_scm {

	}
	| Music_list error {
		Music * m = MY_MAKE_MUSIC("Music");
		// ugh. code dup 
		m->set_property ("error-found", SCM_BOOL_T);
		SCM s = $$;
 		SCM c = scm_cons (m->self_scm (), SCM_EOL);
		scm_gc_unprotect_object (m->self_scm ()); /* UGH */

		if (scm_is_pair (scm_cdr (s)))
			scm_set_cdr_x (scm_cdr (s), c); /* append */
		else
			scm_set_car_x (s, c); /* set first cons */
		scm_set_cdr_x (s, c);  /* remember last cell */
	}
	;

Music:
	Simple_music
	| Composite_music
	;

Alternative_music:
	/* empty */ {
		$$ = SCM_EOL;
	}
	| ALTERNATIVE '{' Music_list '}' {
		$$ = $3;
	}
	;


Repeated_music:
	REPEAT simple_string bare_unsigned Music Alternative_music
	{
		Music *beg = $4;
		int times = $3;
		SCM alts = scm_is_pair ($5) ? scm_car ($5) : SCM_EOL;
		if (times < scm_ilength (alts)) {
		  unsmob_music (scm_car (alts))
		    ->origin ()->warning (
		    _ ("More alternatives than repeats.  Junking excess alternatives."));
		  alts = ly_truncate_list (times, alts);
		}


		SCM proc = ly_scheme_function ("make-repeated-music");

		SCM mus = scm_call_1 (proc, $2);
		scm_gc_protect_object (mus); // UGH.
		Music *r = unsmob_music (mus);
		if (beg)
			{
			r-> set_property ("element", beg->self_scm ());
			scm_gc_unprotect_object (beg->self_scm ());
			}
		r->set_property ("repeat-count", scm_int2num (times >? 1));

		r-> set_property ("elements",alts);
		if (ly_c_equal_p ($2, scm_makfrom0str ("tremolo"))) {
			/*
			TODO: move this code to Scheme.
			*/

			/*
			we can not get durations and other stuff correct down the line, so we have to
			add to the duration log here.
			*/
			SCM func = ly_scheme_function ("shift-duration-log");

			int dots = ($3 % 3) ? 0 : 1;
			int shift = -intlog2 ((dots) ? ($3*2/3) : $3);

			Sequential_music *seq = dynamic_cast<Sequential_music*> ($4);
			
			if (seq) {
				int list_len = scm_ilength (seq->music_list ());
				if (list_len != 2)
					seq->origin ()->warning ("Chord tremolo must have 2 elements.");
				shift -= 1;
				r->compress (Moment (Rational (1, list_len)));
				}
			scm_call_3 (func, r->self_scm (), scm_int2num (shift),scm_int2num (dots));

		}
		r->set_spot (*$4->origin ());

		$$ = r;
	}
	;

Sequential_music:
	SEQUENTIAL '{' Music_list '}'		{
		$$ = MY_MAKE_MUSIC ("SequentialMusic");
		$$->set_property ("elements", scm_car ($3));
		$$->set_spot (THIS->here_input ());
	}
	| '{' Music_list '}'		{
		$$ = MY_MAKE_MUSIC ("SequentialMusic");
		$$->set_property ("elements", scm_car ($2));
		$$->set_spot (THIS->here_input ());
	}
	;

Simultaneous_music:
	SIMULTANEOUS '{' Music_list '}'{
		$$ = MY_MAKE_MUSIC ("SimultaneousMusic");
		$$->set_property ("elements", scm_car ($3));
		$$->set_spot (THIS->here_input ());

	}
	| simul_open Music_list simul_close	{
		$$ = MY_MAKE_MUSIC ("SimultaneousMusic");
		$$->set_property ("elements", scm_car ($2));
		$$->set_spot (THIS->here_input ());
	}
	;

Simple_music:
	event_chord		{ $$ = $1; }
	| MUSIC_IDENTIFIER {
		$$ = unsmob_music ($1);
	}
	| music_property_def
	| context_change
	;


optional_context_mod:
	/**/ { $$ = SCM_EOL; }
	| WITH { THIS->lexer_->push_initial_state (); }
	'{' context_mod_list '}'
	{
		THIS->lexer_->pop_state ();
		$$ = $4;
	}
	;

context_mod_list:
	/* */  { $$ = SCM_EOL; }
	| context_mod_list context_mod  {
		 $$ = scm_cons ($2, $1);
	}
	;


Composite_music:
	Prefix_composite_music { $$ = $1; }
	| Grouped_music_list { $$ = $1; }
	;

Grouped_music_list:
	Simultaneous_music		{ $$ = $1; }
	| Sequential_music		{ $$ = $1; }
	;

Generic_prefix_music_scm:
	MUSIC_FUNCTION {
		$$ = scm_list_2 ($1, make_input (THIS->here_input ()));
	}
	| MUSIC_FUNCTION_SCM {
		THIS->push_spot ();
	} embedded_scm {
		$$ = scm_list_3 ($1, make_input (THIS->pop_spot ()), $3);
	}
	| MUSIC_FUNCTION_MARKUP {
		THIS->push_spot ();
	} full_markup {
		$$ = scm_list_3 ($1, make_input (THIS->pop_spot ()), $3);
	}
	| MUSIC_FUNCTION_MUSIC {
		THIS->push_spot (); 
	} Music {
		$$ = scm_list_3 ($1, make_input (THIS->pop_spot ()), $3->self_scm ());
		scm_gc_unprotect_object ($3->self_scm ());
	}
	| MUSIC_FUNCTION_SCM_MUSIC {
		THIS->push_spot (); 
	}  embedded_scm Music {
		$$ = scm_list_4 ($1, make_input (THIS->pop_spot ()), $3, $4->self_scm ());
		scm_gc_unprotect_object ($4->self_scm ());
	}
	| MUSIC_FUNCTION_SCM_SCM {
		THIS->push_spot (); 
	}  embedded_scm embedded_scm {
		$$ = scm_list_4 ($1, make_input (THIS->pop_spot ()), $3, $4);
	}
	| MUSIC_FUNCTION_MARKUP_MUSIC {
		THIS->push_spot (); 
	}  full_markup Music {
		$$ = scm_list_4 ($1, make_input (THIS->pop_spot ()), $3, $4->self_scm ());
		scm_gc_unprotect_object ($4->self_scm ());
	}
	| MUSIC_FUNCTION_MARKUP_MARKUP {
		THIS->push_spot (); 
	}  full_markup full_markup {
		$$ = scm_list_4 ($1, make_input (THIS->pop_spot ()), $3, $4);
	}
	| MUSIC_FUNCTION_MUSIC_MUSIC {
		THIS->push_spot (); 
	}  Music  Music {
		$$ = scm_list_4 ($1, make_input (THIS->pop_spot ()), $3->self_scm (), $4->self_scm ());
		scm_gc_unprotect_object ($3->self_scm ());
		scm_gc_unprotect_object ($4->self_scm ());
	}
	| MUSIC_FUNCTION_SCM_MUSIC_MUSIC {
		THIS->push_spot (); 
	} embedded_scm Music Music {
		$$ = scm_list_5 ($1, make_input (THIS->pop_spot ()),
			$3, $4->self_scm (), $5->self_scm ());
		scm_gc_unprotect_object ($5->self_scm ());
		scm_gc_unprotect_object ($4->self_scm ());
	}
	| MUSIC_FUNCTION_MARKUP_MUSIC_MUSIC {
		THIS->push_spot (); 
	} full_markup Music Music {
		$$ = scm_list_5 ($1, make_input (THIS->pop_spot ()),
			$3, $4->self_scm (), $5->self_scm ());
		scm_gc_unprotect_object ($5->self_scm ());
		scm_gc_unprotect_object ($4->self_scm ());
	}
	;

Generic_prefix_music:
	Generic_prefix_music_scm {
		SCM func = scm_car ($1);
		Input *loc = unsmob_input (scm_cadr ($1));
		SCM args = scm_cddr ($1);
		SCM sig = scm_object_property (func, ly_symbol2scm ("music-function-signature"));
		int k = 0;
		bool ok  = true; 
		for (SCM s = sig, t = args;
			ok && scm_is_pair (s) && scm_is_pair (t);
			s = scm_cdr (s), t = scm_cdr (t)) {
			k++;
			if (scm_call_1 (scm_car (s), scm_car (t)) != SCM_BOOL_T)
			{
				loc->error (_f ("Argument %d failed typecheck", k));
				THIS->error_level_ = 1;
				ok = false;
			}
		}
		SCM m = SCM_EOL;
		if (ok)
			m = scm_apply_0 (func, scm_cdr ($1));
		if (unsmob_music (m))
			{
			$$ = unsmob_music (m);
			scm_gc_protect_object (m);
			}
		else 
			{
			if (ok)
 				loc->error (_ ("Music head function should return Music object.")); 
			$$ = MY_MAKE_MUSIC ("Music");
			}
		$$->set_spot (*loc);
	}
	;


Prefix_composite_music:
	Generic_prefix_music {
		$$ = $1;
	}
	| CONTEXT simple_string '=' simple_string optional_context_mod Music {
		$$ = context_spec_music ($2, $4, $6, $5);

	}
	| CONTEXT simple_string optional_context_mod Music {
		$$ = context_spec_music ($2, SCM_UNDEFINED, $4, $3);
	}
	| NEWCONTEXT simple_string optional_context_mod Music {
		$$ = context_spec_music ($2, get_next_unique_context (), $4,
			$3);
	}

	| TIMES {
		THIS->push_spot ();
	}
	/* CONTINUED */
		fraction Music 	

	{
		int n = scm_to_int (scm_car ($3)); int d = scm_to_int (scm_cdr ($3));
		Music *mp = $4;

		$$= MY_MAKE_MUSIC ("TimeScaledMusic");
		$$->set_spot (THIS->pop_spot ());

		$$->set_property ("element", mp->self_scm ());
		scm_gc_unprotect_object (mp->self_scm ());
		$$->set_property ("numerator", scm_int2num (n));
		$$->set_property ("denominator", scm_int2num (d));
		$$->compress (Moment (Rational (n,d)));

	}
	| Repeated_music		{ $$ = $1; }
	| TRANSPOSE pitch_also_in_chords pitch_also_in_chords Music {
		$$ = MY_MAKE_MUSIC ("TransposedMusic");
		Music *p = $4;
		Pitch from = *unsmob_pitch ($2);
		Pitch to = *unsmob_pitch ($3);

		p->transpose (pitch_interval (from, to));
		$$->set_property ("element", p->self_scm ());
		scm_gc_unprotect_object (p->self_scm ());
	}
	| mode_changing_head Grouped_music_list {
		if ($1 == ly_symbol2scm ("chords"))
		{
		  Music *chm = MY_MAKE_MUSIC ("UnrelativableMusic");
		  chm->set_property ("element", $2->self_scm ());
		  $$ = chm;
		  scm_gc_unprotect_object ($2->self_scm ());
		}
		else
		{
		  $$ = $2;
		}
		THIS->lexer_->pop_state ();
	}
	| mode_changing_head_with_context optional_context_mod Grouped_music_list {
		$$ = context_spec_music ($1, get_next_unique_context (),
					 $3, $2);
		if ($1 == ly_symbol2scm ("ChordNames"))
		{
		  Music *chm = MY_MAKE_MUSIC ("UnrelativableMusic");
		  chm->set_property ("element", $$->self_scm ());
		  scm_gc_unprotect_object ($$->self_scm ());
		  $$ = chm;
		}
		THIS->lexer_->pop_state ();
	}
	| relative_music	{ $$ = $1; }
	| re_rhythmed_music	{ $$ = $1; }
	| TAG embedded_scm Music {
		tag_music ($3, $2, THIS->here_input ());
		$$ = $3;
	}
	;

mode_changing_head: 
	NOTEMODE {
		SCM nn = THIS->lexer_->lookup_identifier ("pitchnames");
		THIS->lexer_->push_note_state (alist_to_hashq (nn));

		$$ = ly_symbol2scm ("notes");
	}
	| DRUMMODE 
		{
		SCM nn = THIS->lexer_->lookup_identifier ("drumPitchNames");
		THIS->lexer_->push_note_state (alist_to_hashq (nn));

		$$ = ly_symbol2scm ("drums");
	}
	| FIGUREMODE {
		THIS->lexer_->push_figuredbass_state ();

		$$ = ly_symbol2scm ("figures");
	}
	| CHORDMODE {
		SCM nn = THIS->lexer_->lookup_identifier ("chordmodifiers");
		THIS->lexer_->chordmodifier_tab_ = alist_to_hashq (nn);
		nn = THIS->lexer_->lookup_identifier ("pitchnames");
		THIS->lexer_->push_chord_state (alist_to_hashq (nn));
		$$ = ly_symbol2scm ("chords");

	}
	| LYRICMODE
		{ THIS->lexer_->push_lyric_state ();
		$$ = ly_symbol2scm ("lyrics");
	}
	;

mode_changing_head_with_context: 
	DRUMS {
		SCM nn = THIS->lexer_->lookup_identifier ("drumPitchNames");
		THIS->lexer_->push_note_state (alist_to_hashq (nn));

		$$ = ly_symbol2scm ("DrumStaff");
	}
	| FIGURES {
		THIS->lexer_->push_figuredbass_state ();

		$$ = ly_symbol2scm ("FiguredBass");
	}
	| CHORDS {
		SCM nn = THIS->lexer_->lookup_identifier ("chordmodifiers");
		THIS->lexer_->chordmodifier_tab_ = alist_to_hashq (nn);
		nn = THIS->lexer_->lookup_identifier ("pitchnames");
		THIS->lexer_->push_chord_state (alist_to_hashq (nn));
		$$ = ly_symbol2scm ("ChordNames");
	}
	| LYRICS
		{ THIS->lexer_->push_lyric_state ();
		$$ = ly_symbol2scm ("Lyrics");
	}
	;


relative_music:
	RELATIVE absolute_pitch Music {
		Music *m = $3;
		Pitch start = *unsmob_pitch ($2);
		$$ = make_music_relative (start, m);
		scm_gc_unprotect_object (m->self_scm ());
	}
	| RELATIVE Composite_music {
		Music *m = $2;

		Pitch middle_c (0, 0, 0);
		$$ = make_music_relative (middle_c, m);
		scm_gc_unprotect_object (m->self_scm ());
	}
	;

new_lyrics:
	ADDLYRICS { THIS->lexer_->push_lyric_state (); }
	/*cont */
	Grouped_music_list {
	/* Can also use Music at the expensive of two S/Rs similar to
           \repeat \alternative */
		THIS->lexer_->pop_state ();

		$$ = scm_cons ($3->self_scm (), SCM_EOL);
	}
	| new_lyrics ADDLYRICS {
		THIS->lexer_->push_lyric_state ();
	} Grouped_music_list {
		THIS->lexer_->pop_state ();
		$$ = scm_cons ($4->self_scm (), $1);
	}
	;

re_rhythmed_music:
	Grouped_music_list new_lyrics {
		Music * voice = $1;
		SCM name = get_first_context_id (scm_makfrom0str ("Voice"), voice); 
		if (!scm_is_string (name))
		{
			name = get_next_unique_context ();
			voice = context_spec_music (scm_makfrom0str ("Voice"),
						    name,
						    voice, SCM_EOL);
		}

		SCM context = scm_makfrom0str ("Lyrics");
		Music *all = MY_MAKE_MUSIC ("SimultaneousMusic");

		SCM lst = SCM_EOL;
		for (SCM s = $2; scm_is_pair (s); s = scm_cdr (s))
		{
			Music *music = unsmob_music (scm_car (s));
			Music *com = make_lyric_combine_music (name, music);
			Music *csm = context_spec_music (context,
				get_next_unique_context (), com, SCM_EOL);
			lst = scm_cons (csm->self_scm (), lst);
		}
		all->set_property ("elements", scm_cons (voice->self_scm (),
			lst));
		$$ = all;
		scm_gc_unprotect_object (voice->self_scm ());
	}
	| LYRICSTO {
		THIS->lexer_->push_lyric_state ();
	} simple_string Music {
		THIS->lexer_->pop_state ();
		Music *music = $4;
		SCM name = $3;
		$$ = make_lyric_combine_music (name, music);
		scm_gc_unprotect_object (music->self_scm ());
	}
	;

context_change:
	CHANGE STRING '=' STRING  {
		Music*t= MY_MAKE_MUSIC ("ContextChange");
		t-> set_property ("change-to-type", scm_string_to_symbol ($2));
		t-> set_property ("change-to-id", $4);

		$$ = t;
		$$->set_spot (THIS->here_input ());
	}
	;

property_operation:
	STRING '=' scalar {
		$$ = scm_list_3 (ly_symbol2scm ("assign"),
			scm_string_to_symbol ($1), $3);
	}
	| UNSET simple_string {
		$$ = scm_list_2 (ly_symbol2scm ("unset"),
			scm_string_to_symbol ($2));
	}
	| OVERRIDE simple_string embedded_scm '=' embedded_scm {
		$$ = scm_list_4 (ly_symbol2scm ("push"),
			scm_string_to_symbol ($2), $3, $5);
	}
	| REVERT simple_string embedded_scm {
		$$ = scm_list_3 (ly_symbol2scm ("pop"),
			scm_string_to_symbol ($2), $3);
	}
	;

context_def_mod:
	CONSISTS { $$ = ly_symbol2scm ("consists"); }
	| REMOVE { $$ = ly_symbol2scm ("remove"); }

	| ACCEPTS { $$ = ly_symbol2scm ("accepts"); }
	| DENIES { $$ = ly_symbol2scm ("denies"); }

	| ALIAS { $$ = ly_symbol2scm ("alias"); }
	| TYPE { $$ = ly_symbol2scm ("translator-type"); }
	| DESCRIPTION { $$ = ly_symbol2scm ("description"); }
	| NAME { $$ = ly_symbol2scm ("context-name"); }
	;

context_mod:
	property_operation { $$ = $1; }
	| context_def_mod STRING {
		$$ = scm_list_2 ($1, $2);
	}
	;

context_prop_spec:
	simple_string {
		if (!is_regular_identifier ($1))
		{
			THIS->here_input ().error (_("Grob name should be alphanumeric"));
		}

		$$ = scm_list_2 (ly_symbol2scm ("Bottom"),
			scm_string_to_symbol ($1));
	}
	| simple_string '.' simple_string {
		$$ = scm_list_2 (scm_string_to_symbol ($1),
			scm_string_to_symbol ($3));
	}
	;

music_property_def:
	OVERRIDE context_prop_spec embedded_scm '=' scalar {
		$$ = property_op_to_music (scm_list_4 (
			ly_symbol2scm ("poppush"),
			scm_cadr ($2),
			$3, $5));
		$$= context_spec_music (scm_car ($2), SCM_UNDEFINED, $$, SCM_EOL);
	}
	| REVERT context_prop_spec embedded_scm {
		$$ = property_op_to_music (scm_list_3 (
			ly_symbol2scm ("pop"),
			scm_cadr ($2),
			$3));

		$$= context_spec_music (scm_car ($2), SCM_UNDEFINED, $$, SCM_EOL);
	}
	| SET context_prop_spec '=' scalar {
		$$ = property_op_to_music (scm_list_3 (
			ly_symbol2scm ("assign"),
			scm_cadr ($2),
			$4));
		$$= context_spec_music (scm_car ($2), SCM_UNDEFINED, $$, SCM_EOL);
	}
	| UNSET context_prop_spec {
		$$ = property_op_to_music (scm_list_2 (
			ly_symbol2scm ("unset"),
			scm_cadr ($2)));
		$$= context_spec_music (scm_car ($2), SCM_UNDEFINED, $$, SCM_EOL);
	}
	| ONCE music_property_def {
		SCM e = $2->get_property ("element");
                unsmob_music (e)->set_property ("once", SCM_BOOL_T);
		$$ = $2;

	}
	;


string:
	STRING {
		$$ = $1;
	}
	| STRING_IDENTIFIER {
		$$ = $1;
	}
	| string '+' string {
		$$ = scm_string_append (scm_list_2 ($1, $3));
	}
	;

simple_string: STRING {
		$$ = $1;
	}
	| LYRICS_STRING {
		$$ = $1;
	}
	;

scalar: string {
		$$ = $1;
	}
	| LYRICS_STRING {
		$$ = $1;
	}
        | bare_int {
		$$ = scm_int2num ($1);
	}
        | embedded_scm {
		$$ = $1;
	}
	| full_markup {
		$$ = $1;
	}
	| DIGIT {
		$$ = scm_int2num ($1);
	}
	;

/*

pre_events doesn't contain anything. It is a trick:

Adding pre_events to the simple_element
makes the choice between

  string:  STRING

and

  simple_element: STRING

a single shift/reduction conflict.

nevertheless, this is not very clean, and we should find a different
solution.

*/
pre_events: {
		THIS->push_spot ();
	}
	;

event_chord:
	pre_events simple_element post_events	{
		SCM elts = $2-> get_property ("elements");

		elts = ly_append2 (elts, scm_reverse_x ($3, SCM_EOL));

		$2->set_property ("elements", elts);
		$$ = $2;
	}
	| command_element
	| note_chord_element
	;


note_chord_element:
	chord_body optional_notemode_duration post_events
	{
		SCM dur = unsmob_duration ($2)->smobbed_copy ();
		SCM es = $1->get_property ("elements");
		SCM postevs = scm_reverse_x ($3, SCM_EOL);

		for (SCM s = es; scm_is_pair (s); s = scm_cdr (s))
		  unsmob_music (scm_car (s))->set_property ("duration", dur);
		es = ly_append2 (es, postevs);

		$1-> set_property ("elements", es);
		$$ = $1;
	}
	;

chord_open: '<'
	;

chord_close: '>'
	;

simul_open: DOUBLE_ANGLE_OPEN
	;

simul_close: DOUBLE_ANGLE_CLOSE
	;

chord_body:
	chord_open chord_body_elements chord_close
	{
		$$ = MY_MAKE_MUSIC ("EventChord");
		$$->set_property ("elements",
			scm_reverse_x ($2, SCM_EOL));
	}
	;

chord_body_elements:
	/* empty */ 		{ $$ = SCM_EOL; }
	| chord_body_elements chord_body_element {
		$$ = scm_cons ($2->self_scm (), $1);
		scm_gc_unprotect_object ($2->self_scm ());
	}
	;

chord_body_element:
	pitch  exclamations questions octave_check post_events
	{
		int q = $3;
		int ex = $2;
		SCM check = $4;
		SCM post = $5;

		Music *n = MY_MAKE_MUSIC ("NoteEvent");
		n->set_property ("pitch", $1);
		if (q % 2)
			n->set_property ("cautionary", SCM_BOOL_T);
		if (ex % 2 || q % 2)
			n->set_property ("force-accidental", SCM_BOOL_T);

		if (scm_is_pair (post)) {
			SCM arts = scm_reverse_x (post, SCM_EOL);
			n->set_property ("articulations", arts);
		}
		if (scm_is_number (check))
		{
			int q = scm_to_int (check);
			n->set_property ("absolute-octave", scm_int2num (q-1));
		}

		
		$$ = n;
	}
	| DRUM_PITCH post_events {
		Music *n = MY_MAKE_MUSIC ("NoteEvent");
		n->set_property ("duration", $2);
		n->set_property ("drum-type", $1);
		n->set_spot (THIS->here_input ());

		if (scm_is_pair ($2)) {
			SCM arts = scm_reverse_x ($2, SCM_EOL);
			n->set_property ("articulations", arts);
		}
		$$ = n;
	}
	;

add_quote:
	ADDQUOTE string Music {
		SCM adder = ly_scheme_function ("add-quotable");
		
		scm_call_2 (adder, $2, $3->self_scm ());
		scm_gc_unprotect_object ($3->self_scm ());
	}
	;

command_element:
	command_req {
		$$ = MY_MAKE_MUSIC ("EventChord");
		$$->set_property ("elements", scm_cons ($1->self_scm (), SCM_EOL));
		scm_gc_unprotect_object ($1->self_scm ());

		$$-> set_spot (THIS->here_input ());
		$1-> set_spot (THIS->here_input ());
	}
	| SKIP duration_length {
		Music *skip = MY_MAKE_MUSIC ("SkipMusic");
		skip->set_property ("duration", $2);

		$$ = skip;
	}
	| QUOTE STRING duration_length {
		Music *quote = MY_MAKE_MUSIC ("QuoteMusic");
		quote->set_property ("duration", $3);
		quote->set_property ("quoted-music-name", $2);
		quote->set_spot (THIS->here_input ());

		$$ = quote;
	}
	| OCTAVE { THIS->push_spot (); }
 	  pitch {
		Music *m = MY_MAKE_MUSIC ("RelativeOctaveCheck");
		$$ = m;
		$$->set_spot (THIS->pop_spot ());
		$$->set_property ("pitch", $3);
	}
	| E_LEFTSQUARE {
		Music *m = MY_MAKE_MUSIC ("LigatureEvent");
		m->set_property ("span-direction", scm_int2num (START));
		m->set_spot (THIS->here_input ());

		$$ = MY_MAKE_MUSIC ("EventChord");
		$$->set_property ("elements", scm_cons (m->self_scm (), SCM_EOL));
		scm_gc_unprotect_object (m->self_scm ());
		$$->set_spot (THIS->here_input ());
	}
	| E_RIGHTSQUARE {
		Music *m = MY_MAKE_MUSIC ("LigatureEvent");
		m->set_property ("span-direction", scm_int2num (STOP));
		m->set_spot (THIS->here_input ());

		$$ = MY_MAKE_MUSIC ("EventChord");
		$$->set_property ("elements", scm_cons (m->self_scm (), SCM_EOL));
		$$->set_spot (THIS->here_input ());
		scm_gc_unprotect_object (m->self_scm ());
	}
	| E_BACKSLASH {
		$$ = MY_MAKE_MUSIC ("VoiceSeparator");
		$$->set_spot (THIS->here_input ());
	}
	| '|'      {
		SCM pipe =THIS->lexer_->lookup_identifier ("pipeSymbol");

		if (Music * m = unsmob_music (pipe))
			$$ = m->clone ();
		else
			$$ = MY_MAKE_MUSIC ("BarCheck");

		$$->set_spot (THIS->here_input ());
	}
	| TRANSPOSITION pitch {
		Pitch middle_c;
		Pitch sounds_as_c = pitch_interval (*unsmob_pitch ($2), middle_c);
		$$ = set_property_music (ly_symbol2scm ("instrumentTransposition"),
					 sounds_as_c.smobbed_copy());
		$$->set_spot (THIS-> here_input ());
		$$ = context_spec_music (ly_symbol2scm ("Staff"), SCM_UNDEFINED,
			$$, SCM_EOL);
	}
	| BAR STRING  			{
		Music *t = set_property_music (ly_symbol2scm ("whichBar"), $2);

		Music *csm = context_spec_music (ly_symbol2scm ("Timing"), SCM_UNDEFINED,
					t, SCM_EOL);
		$$ = context_spec_music (ly_symbol2scm ("Score"), SCM_UNDEFINED, csm, SCM_EOL);
		$$->set_spot (THIS->here_input ());
		t->set_spot (THIS->here_input ());
	}
	| PARTIAL duration_length  	{
		Moment m = - unsmob_duration ($2)->get_length ();
		Music *p = set_property_music (ly_symbol2scm ( "measurePosition"),m.smobbed_copy ());
		p->set_spot (THIS->here_input ());
		p = context_spec_music (ly_symbol2scm ("Timing"), SCM_UNDEFINED,
					p, SCM_EOL);
		p = context_spec_music (ly_symbol2scm ("Score"), SCM_UNDEFINED,
					p, SCM_EOL);
		$$ = p;
	}
	| CLEF STRING  {
		SCM proc = ly_scheme_function ("make-clef-set");

		SCM result = scm_call_1 (proc, $2);
		scm_gc_protect_object (result);
		$$ = unsmob_music (result);
	}
	| TIME_T fraction  {
		SCM proc= ly_scheme_function ("make-time-signature-set");

		SCM result = scm_apply_2   (proc, scm_car ($2), scm_cdr ($2), SCM_EOL);
		scm_gc_protect_object (result);
		$$ = unsmob_music (result);
	}
	| MARK scalar {
		SCM proc = ly_scheme_function ("make-mark-set");

		SCM result = scm_call_1 (proc, $2);
		scm_gc_protect_object (result);
		$$ = unsmob_music (result);
	}
	;

command_req:
	E_TILDE {
		$$ = MY_MAKE_MUSIC ("PesOrFlexaEvent");
	}
	| MARK DEFAULT  {
		Music *m = MY_MAKE_MUSIC ("MarkEvent");
		$$ = m;
	}
	| tempo_event {
		$$ = $1;
	}
	| KEY DEFAULT {
		Music *key= MY_MAKE_MUSIC ("KeyChangeEvent");
		$$ = key;
	}
	| KEY NOTENAME_PITCH SCM_IDENTIFIER 	{

		Music *key= MY_MAKE_MUSIC ("KeyChangeEvent");
		if (scm_ilength ($3) > 0)
		{		
			key->set_property ("pitch-alist", $3);
			key->set_property ("tonic", Pitch (0,0,0).smobbed_copy ());
			((Music*)key)->transpose (* unsmob_pitch ($2));
		} else {
			THIS->parser_error (_ ("Second argument must be pitch list."));
		}

		$$ = key;
	}
	;

post_events:
	/* empty */ {
		$$ = SCM_EOL;
	}
	| post_events post_event {
		$2->set_spot (THIS->here_input ());
		$$ = scm_cons ($2->self_scm (), $$);
		scm_gc_unprotect_object ($2->self_scm ());
	}
	| post_events tagged_post_event {
		$2 -> set_spot (THIS->here_input ());
		$$ = scm_cons ($2->self_scm (), $$);
		scm_gc_unprotect_object ($2->self_scm ());
	}
	;


tagged_post_event:
	'-' TAG embedded_scm post_event {
		tag_music ($4, $3, THIS->here_input ());
		$$ = $4;
	}
	;

post_event:
	direction_less_event {
		$$ = $1;
	}
	| HYPHEN {
		if (!THIS->lexer_->is_lyric_state ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = MY_MAKE_MUSIC ("HyphenEvent");
	}
	| EXTENDER {
		if (!THIS->lexer_->is_lyric_state ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = MY_MAKE_MUSIC ("ExtenderEvent");
	}
	| script_dir direction_reqd_event {
		if ($1)
			$2->set_property ("direction", scm_int2num ($1));
		$$ = $2;
	}
	| script_dir direction_less_event {
		if ($1)
			$2->set_property ("direction", scm_int2num ($1));
		$$ = $2;
	}
	| string_number_event
	;

string_number_event:
	E_UNSIGNED {
		Music *s = MY_MAKE_MUSIC ("StringNumberEvent");
		s->set_property ("string-number", scm_int2num ($1));
		s->set_spot (THIS->here_input ());
		$$ = s;
	}
	;

direction_less_char:
	'['  {
		$$ = ly_symbol2scm ("bracketOpenSymbol");
	}
	| ']'  {
		$$ = ly_symbol2scm ("bracketCloseSymbol"); 
	}
	| '~'  {
		$$ = ly_symbol2scm ("tildeSymbol");
	}
	| '('  {
		$$ = ly_symbol2scm ("parenthesisOpenSymbol");
	}
	| ')'  {
		$$ = ly_symbol2scm ("parenthesisCloseSymbol");
	}
	| E_EXCLAMATION  {
		$$ = ly_symbol2scm ("escapedExclamationSymbol");
	}
	| E_OPEN  {
		$$ = ly_symbol2scm ("escapedParenthesisOpenSymbol");
	}
	| E_CLOSE  {
		$$ = ly_symbol2scm ("escapedParenthesisCloseSymbol");
	}
	| E_BIGGER  {
		$$ = ly_symbol2scm ("escapedBiggerSymbol");
	}
	| E_SMALLER  {
		$$ = ly_symbol2scm ("escapedSmallerSymbol");
	}
	;

direction_less_event:
	direction_less_char {
		SCM predefd = THIS->lexer_->lookup_identifier_symbol ($1);
		Music * m = 0;
		if (unsmob_music (predefd))
		{
			m = unsmob_music (predefd)->clone ();
		}
		else
		{
			m = MY_MAKE_MUSIC ("Music");
		}
		m->set_spot (THIS->here_input ());
		$$ = m;		
	}
	| EVENT_IDENTIFIER	{
		$$ = unsmob_music ($1);
	}
	| tremolo_type  {
               Music *a = MY_MAKE_MUSIC ("TremoloEvent");
               a->set_spot (THIS->here_input ());
               a->set_property ("tremolo-type", scm_int2num ($1));
               $$ = a;
        }
	;	
	
direction_reqd_event:
	gen_text_def {
		$$ = $1;
	}
	| script_abbreviation {
		SCM s = THIS->lexer_->lookup_identifier ("dash" + ly_scm2string ($1));
		Music *a = MY_MAKE_MUSIC ("ArticulationEvent");
		if (scm_is_string (s))
			a->set_property ("articulation-type", s);
		else THIS->parser_error (_ ("Expecting string as script definition"));
		$$ = a;
	}
	;

octave_check:
	/**/ { $$ = SCM_EOL; }
	| '='  { $$ = scm_int2num (0); }
	| '=' sub_quotes { $$ = scm_int2num ($2); }
	| '=' sup_quotes { $$ = scm_int2num ($2); }
	;

sup_quotes:
	'\'' {
		$$ = 1;
	}
	| sup_quotes '\'' {
		$$ ++;
	}
	;

sub_quotes:
	',' {
		$$ = 1;
	}
	| sub_quotes ',' {
		$$++;
	}
	;

steno_pitch:
	NOTENAME_PITCH	{
		$$ = $1;
	}
	| NOTENAME_PITCH sup_quotes 	{
		Pitch p = *unsmob_pitch ($1);
		p = p.transposed (Pitch ($2,0,0));
		$$ = p.smobbed_copy ();
	}
	| NOTENAME_PITCH sub_quotes	 {
		Pitch p =* unsmob_pitch ($1);
		p = p.transposed (Pitch (-$2,0,0));
		$$ = p.smobbed_copy ();
	}
	;

/*
ugh. duplication
*/

steno_tonic_pitch:
	TONICNAME_PITCH	{
		$$ = $1;
	}
	| TONICNAME_PITCH sup_quotes 	{
		Pitch p = *unsmob_pitch ($1);
		p = p.transposed (Pitch ($2,0,0));
		$$ = p.smobbed_copy ();
	}
	| TONICNAME_PITCH sub_quotes	 {
		Pitch p =* unsmob_pitch ($1);

		p = p.transposed (Pitch (-$2,0,0));
		$$ = p.smobbed_copy ();
	}
	;

pitch:
	steno_pitch {
		$$ = $1;
	}
	;

pitch_also_in_chords:
	pitch
	| steno_tonic_pitch
	;

gen_text_def:
	full_markup {
		Music *t = MY_MAKE_MUSIC ("TextScriptEvent");
		t->set_property ("text", $1);
		t->set_spot (THIS->here_input ());
		$$ = t;	
	}
	| string {
		Music *t = MY_MAKE_MUSIC ("TextScriptEvent");
		t->set_property ("text",
			make_simple_markup (THIS->lexer_->encoding (), $1));
		t->set_spot (THIS->here_input ());
		$$ = t;
	
	}
	| DIGIT {
		Music *t = MY_MAKE_MUSIC ("FingerEvent");
		t->set_property ("digit", scm_int2num ($1));
		t->set_spot (THIS->here_input ());
		$$ = t;
	}
	;

script_abbreviation:
	'^'		{
		$$ = scm_makfrom0str ("Hat");
	}
	| '+'		{
		$$ = scm_makfrom0str ("Plus");
	}
	| '-' 		{
		$$ = scm_makfrom0str ("Dash");
	}
 	| '|'		{
		$$ = scm_makfrom0str ("Bar");
	}
	| '>'		{
		$$ = scm_makfrom0str ("Larger");
	}
	| '.' 		{
		$$ = scm_makfrom0str ("Dot");
	}
	| '_' {
		$$ = scm_makfrom0str ("Underscore");
	}
	;

script_dir:
	'_'	{ $$ = DOWN; }
	| '^'	{ $$ = UP; }
	| '-'	{ $$ = CENTER; }
	;


absolute_pitch:
	steno_pitch	{
		$$ = $1;
	}
	;

duration_length:
	multiplied_duration {
		$$ = $1;
	}
	;

optional_notemode_duration:
	{
		Duration dd = THIS->default_duration_;
		$$ = dd.smobbed_copy ();

		THIS->beam_check ($$);
	}
	| multiplied_duration	{
		$$ = $1;
		THIS->default_duration_ = *unsmob_duration ($$);

		THIS->beam_check ($$);
	}
	;

steno_duration:
	bare_unsigned dots		{
		int len = 0;
		if (!is_duration ($1))
			THIS->parser_error (_f ("not a duration: %d", $1));
		else
			len = intlog2 ($1);

		$$ = Duration (len, $2).smobbed_copy ();
	}
	| DURATION_IDENTIFIER dots	{
		Duration *d = unsmob_duration ($1);
		Duration k (d->duration_log (), d->dot_count () + $2);
		*d = k;
		$$ = $1;
	}
	;

multiplied_duration:
	steno_duration {
		$$ = $1;
	}
	| multiplied_duration '*' bare_unsigned {
		$$ = unsmob_duration ($$)->compressed ( $3) .smobbed_copy ();
	}
	| multiplied_duration '*' FRACTION {
		Rational  m (scm_to_int (scm_car ($3)), scm_to_int (scm_cdr ($3)));

		$$ = unsmob_duration ($$)->compressed (m).smobbed_copy ();
	}
	;

fraction:
	FRACTION { $$ = $1; }
	| UNSIGNED '/' UNSIGNED {
		$$ = scm_cons (scm_int2num ($1), scm_int2num ($3));
	}
	;

dots:
	/* empty */ 	{
		$$ = 0;
	}
	| dots '.' {
		$$ ++;
	}
	;

tremolo_type:
	':'	{
		$$ = 0;
	}
	| ':' bare_unsigned {
		if (!is_duration ($2))
			THIS->parser_error (_f ("not a duration: %d", $2));
		$$ = $2;
	}
	;

bass_number:
	DIGIT   {
		$$ = scm_number_to_string (scm_int2num ($1), scm_int2num (10));
		$$ = scm_list_2 (ly_scheme_function ("number-markup"),
				$$);
	}
	| UNSIGNED {
		$$ = scm_number_to_string (scm_int2num ($1), scm_int2num (10));
		$$ = scm_list_2 (ly_scheme_function ("number-markup"),
				$$);
	}
	| STRING { $$ = $1; }
	;

bass_mod:
	'-' 	{ $$ = -2; }
	| '+'	{ $$ = 2; }
	| '!'	{ $$ = 0; }
	;

bass_figure:
	FIGURE_SPACE {
		Music *bfr = MY_MAKE_MUSIC ("BassFigureEvent");
		$$ = bfr->self_scm ();
		scm_gc_unprotect_object ($$);
	}
	| bass_number  {
		Music *bfr = MY_MAKE_MUSIC ("BassFigureEvent");
		$$ = bfr->self_scm ();

		bfr->set_property ("figure", $1);

		scm_gc_unprotect_object ($$);
	}
	| bass_figure bass_mod {
		Music *m = unsmob_music ($1);
		if ($2) {
			SCM salter = m->get_property ("alteration");
			int alter = scm_is_number (salter) ? scm_to_int (salter) : 0;
			m->set_property ("alteration",
				scm_int2num (alter + $2));
		} else {
			m->set_property ("alteration", scm_int2num (0));
		}
	}
	;

br_bass_figure:
	'[' bass_figure {
		$$ = $2;
		unsmob_music ($$)->set_property ("bracket-start", SCM_BOOL_T);
	}
	| bass_figure	{
		$$ = $1;
	}
	| br_bass_figure ']' {
		$$ = $1;
		unsmob_music ($1)->set_property ("bracket-stop", SCM_BOOL_T);
	}
	;

figure_list:
	/**/		{
		$$ = SCM_EOL;
	}
	| figure_list br_bass_figure {
		$$ = scm_cons ($2, $1);
	}
	;

figure_spec:
	FIGURE_OPEN figure_list FIGURE_CLOSE {
		Music *m = MY_MAKE_MUSIC ("EventChord");
		$2 = scm_reverse_x ($2, SCM_EOL);
		m->set_property ("elements", $2);
		$$ = m->self_scm ();
	}
	;


optional_rest:
	/**/   { $$ = 0; }
	| REST { $$ = 1; }
	;

simple_element:
	pitch exclamations questions octave_check optional_notemode_duration optional_rest {

		Input i = THIS->pop_spot ();
		if (!THIS->lexer_->is_note_state ())
			THIS->parser_error (_ ("Have to be in Note mode for notes"));

		Music *n = 0;
		if ($6)
			n = MY_MAKE_MUSIC ("RestEvent");
		else
			n = MY_MAKE_MUSIC ("NoteEvent");
		
		n->set_property ("pitch", $1);
		n->set_property ("duration", $5);

		if (scm_is_number ($4))
		{
			int q = scm_to_int ($4);
			n->set_property ("absolute-octave", scm_int2num (q-1));
		}

		if ($3 % 2)
			n->set_property ("cautionary", SCM_BOOL_T);
		if ($2 % 2 || $3 % 2)
			n->set_property ("force-accidental", SCM_BOOL_T);

		Music *v = MY_MAKE_MUSIC ("EventChord");
		v->set_property ("elements", scm_list_1 (n->self_scm ()));
		scm_gc_unprotect_object (n->self_scm ());

		v->set_spot (i);
		n->set_spot (i);
		$$ = v;
	}
	| DRUM_PITCH optional_notemode_duration {
		Input i = THIS->pop_spot ();

		Music *n = MY_MAKE_MUSIC ("NoteEvent");
		n->set_property ("duration", $2);
		n->set_property ("drum-type", $1);

		Music *v = MY_MAKE_MUSIC ("EventChord");
		v->set_property ("elements", scm_list_1 (n->self_scm ()));
		scm_gc_unprotect_object (n->self_scm ());
		v->set_spot (i);
		n->set_spot (i);
		$$ = v;
		
	}
	| figure_spec optional_notemode_duration {
		Music *m = unsmob_music ($1);
		Input i = THIS->pop_spot ();
		m->set_spot (i);
		for (SCM s = m->get_property ("elements"); scm_is_pair (s); s = scm_cdr (s))
		{
			unsmob_music (scm_car (s))->set_property ("duration", $2);
		}
		$$ = m;
	}	
 	| RESTNAME optional_notemode_duration		{

		Input i = THIS->pop_spot ();
		Music *ev = 0;
 		if (ly_scm2string ($1) == "s") {
			/* Space */
			ev = MY_MAKE_MUSIC ("SkipEvent");
		  }
		else {
			ev = MY_MAKE_MUSIC ("RestEvent");
		
		    }
		ev->set_property ("duration", $2);
		ev->set_spot (i);
 		Music *velt = MY_MAKE_MUSIC ("EventChord");
		velt->set_property ("elements", scm_list_1 (ev->self_scm ()));
		velt->set_spot (i);

		scm_gc_unprotect_object (ev->self_scm ());

 		$$ = velt;
	}
	| MULTI_MEASURE_REST optional_notemode_duration  	{
		THIS->pop_spot ();

		SCM proc = ly_scheme_function ("make-multi-measure-rest");
		SCM mus = scm_call_2 (proc, $2,
			make_input (THIS->here_input ()));	
		scm_gc_protect_object (mus);
		$$ = unsmob_music (mus);
	}
	
	| lyric_element optional_notemode_duration 	{
		Input i = THIS->pop_spot ();
		if (!THIS->lexer_->is_lyric_state ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));

		Music *lreq = MY_MAKE_MUSIC ("LyricEvent");
                lreq->set_property ("text", $1);
		lreq->set_property ("duration",$2);
		lreq->set_spot (i);
		Music *velt = MY_MAKE_MUSIC ("EventChord");
		velt->set_property ("elements", scm_list_1 (lreq->self_scm ()));

		$$= velt;
	}
	| new_chord {
		THIS->pop_spot ();

                if (!THIS->lexer_->is_chord_state ())
                        THIS->parser_error (_ ("Have to be in Chord mode for chords"));
                $$ = unsmob_music ($1);
	}
	;

lyric_element:
	/* FIXME: lyric flavoured markup would be better */
	full_markup {
		$$ = $1;
	}
	| LYRICS_STRING {
		$$ = make_simple_markup (THIS->lexer_->encoding (), $1);
	}
	;

new_chord:
	steno_tonic_pitch optional_notemode_duration   {
		$$ = make_chord ($1, $2, SCM_EOL);
	}
	| steno_tonic_pitch optional_notemode_duration chord_separator chord_items {
		SCM its = scm_reverse_x ($4, SCM_EOL);
		$$ = make_chord ($1, $2, scm_cons ($3, its));
	}
	;

chord_items:
	/**/ {
		$$ = SCM_EOL;		
	}
	| chord_items chord_item {
		$$ = scm_cons ($2, $$);
	}
	;

chord_separator:
	CHORD_COLON {
		$$ = ly_symbol2scm ("chord-colon");
	}
	| CHORD_CARET {
		$$ = ly_symbol2scm ("chord-caret");
	}
	| CHORD_SLASH steno_tonic_pitch {
 		$$ = scm_list_2 (ly_symbol2scm ("chord-slash"), $2);
	}
	| CHORD_BASS steno_tonic_pitch {
		$$ = scm_list_2 (ly_symbol2scm ("chord-bass"), $2);
	}
	;

chord_item:
	chord_separator {
		$$ = $1;
	}
	| step_numbers {
		$$ = scm_reverse_x ($1, SCM_EOL);
	}
	| CHORD_MODIFIER  {
		$$ = $1;
	}
	;

step_numbers:
	step_number { $$ = scm_cons ($1, SCM_EOL); }
	| step_numbers '.' step_number {
		$$ = scm_cons ($3, $$);
	}
	;

step_number:
	bare_unsigned {
		$$ = make_chord_step ($1, 0);
        }
	| bare_unsigned '+' {
		$$ = make_chord_step ($1, SHARP);
	}
	| bare_unsigned CHORD_MINUS {
		$$ = make_chord_step ($1, FLAT);
	}
	;	

/*
	UTILITIES

TODO: should deprecate in favor of Scheme?

 */
number_expression:
	number_expression '+' number_term {
		$$ = scm_sum ($1, $3);
	}
	| number_expression '-' number_term {
		$$ = scm_difference ($1, $3);
	}
	| number_term
	;

number_term:
	number_factor {
		$$ = $1;
	}
	| number_factor '*' number_factor {
		$$ = scm_product ($1, $3);
	}
	| number_factor '/' number_factor {
		$$ = scm_divide ($1, $3);
	}
	;

number_factor:
	'-'  number_factor { /* %prec UNARY_MINUS */
		$$ = scm_difference ($2, SCM_UNDEFINED);
	}
	| bare_number
	;


bare_number:
	UNSIGNED	{
		$$ = scm_int2num ($1);
	}
	| REAL		{
		$$ = $1;
	}
	| NUMBER_IDENTIFIER		{
		$$ = $1;
	}
	| REAL NUMBER_IDENTIFIER	{
		$$ = scm_make_real (scm_to_double ($1) *scm_to_double ($2));
	}
	| UNSIGNED NUMBER_IDENTIFIER	{
		$$ = scm_make_real ($1 *scm_to_double ($2));
	}
	;


bare_unsigned:
	UNSIGNED {
			$$ = $1;
	}
	| DIGIT {
		$$ = $1;
	}
	;

bare_int:
	bare_number {
		if (scm_integer_p ($1) == SCM_BOOL_T)
		{
			int k = scm_to_int ($1);
			$$ = k;
		} else
		{
			THIS->parser_error (_ ("need integer number arg"));
			$$ = 0;
		}
	}
	| '-' bare_int {
		$$ = -$2;
	}
	;

exclamations:
		{ $$ = 0; }
	| exclamations '!'	{ $$ ++; }
	;

questions:
		{ $$ = 0; }
	| questions '?'	{ $$ ++; }
	;



full_markup:
	MARKUP_IDENTIFIER {
		$$ = $1;
 	}
	| MARKUP
		{ THIS->lexer_->push_markup_state (); }
	markup
		{ $$ = $3;
		  THIS->lexer_->pop_state ();
		}
	;


/*
This should be done more dynamically if possible.
*/
markup:
	STRING {
		$$ = make_simple_markup (THIS->lexer_->encoding (), $1);
	}
	| MARKUP_HEAD_EMPTY {
		$$ = scm_list_1 ($1);
	}
	| MARKUP_HEAD_MARKUP0 markup {
		$$ = scm_list_2 ($1, $2);
	}
	| MARKUP_HEAD_MARKUP0_MARKUP1 markup markup {
		$$ = scm_list_3 ($1, $2, $3);
	}
	| MARKUP_HEAD_SCM0_MARKUP1 SCM_T markup {
		$$ = scm_list_3 ($1, $2, $3);
	}
	| markup_line {
		$$ = $1;
	}
	| MARKUP_HEAD_LIST0 markup_list {
		$$ = scm_list_2 ($1,$2);
	}
	| MARKUP_HEAD_SCM0 embedded_scm {
		$$ = scm_list_2 ($1, $2);
	}
	| MARKUP_HEAD_SCM0_SCM1_MARKUP2 embedded_scm embedded_scm markup {
		$$ = scm_list_4 ($1, $2, $3, $4);
	}
	| MARKUP_HEAD_SCM0_SCM1_SCM2 embedded_scm embedded_scm embedded_scm {
		$$ = scm_list_4 ($1, $2, $3, $4);
	}
	| MARKUP_HEAD_SCM0_SCM1 embedded_scm embedded_scm {
		$$ = scm_list_3 ($1, $2, $3);
	}
	| MARKUP_IDENTIFIER {
		$$ = $1;
	}
	| STRING_IDENTIFIER {
		$$ = $1;
	}
	| SCORE {
		SCM nn = THIS->lexer_->lookup_identifier ("pitchnames");
		THIS->lexer_->push_note_state (alist_to_hashq (nn));
	} '{' score_body '}' {
		Score * sc = $4;
		$$ = scm_list_2 (ly_scheme_function ("score-markup"), sc->self_scm ());
		scm_gc_unprotect_object (sc->self_scm ());
		THIS->lexer_->pop_state ();
	}
	;

markup_list:
	chord_open markup_list_body chord_close { $$ = scm_reverse_x ($2, SCM_EOL); }
	;

markup_line:
	'{' markup_list_body '}' {
		SCM line = ly_scheme_function ("line-markup");
	
		$$ = scm_list_2 (line, scm_reverse_x ($2, SCM_EOL));
	}
	;

markup_list_body:
	/**/ {  $$ = SCM_EOL; }
	| markup_list_body markup {
		$$ = scm_cons ($2, $1);
	}
	;


%%

void
Lily_parser::set_yydebug (bool )
{
#if 0
	yydebug = 1;
#endif
}

void
Lily_parser::do_yyparse ()
{
	yyparse ((void*)this);
}


/*
Should make this optional?    It will also complain when you do

	[s4]

which is entirely legitimate.

Or we can scrap it. Barchecks should detect wrong durations, and
skipTypesetting speeds it up a lot.
*/

void
Lily_parser::beam_check (SCM dur)
{
  Duration *d = unsmob_duration (dur);
  if (unsmob_music (last_beam_start_) && d->duration_log () <= 2)
    {
      Music *m = unsmob_music (last_beam_start_);
      m->origin ()->warning (_ ("Suspect duration found following this beam"));
    }
  last_beam_start_ = SCM_EOL;
}




/*

It is a little strange to have this function in this file, but
otherwise, we have to import music classes into the lexer.

*/
int
Lily_lexer::try_special_identifiers (SCM *destination, SCM sid)
{
	if (scm_is_string (sid)) {
		*destination = sid;
		return STRING_IDENTIFIER;
	} else if (scm_is_number (sid)) {
		*destination = sid;
		return NUMBER_IDENTIFIER;
	} else if (unsmob_context_def (sid)) {
		*destination = unsmob_context_def (sid)->clone_scm ();
		return CONTEXT_DEF_IDENTIFIER;
	} else if (unsmob_score (sid)) {
		Score *score = new Score (*unsmob_score (sid));
		*destination = score->self_scm ();
		return SCORE_IDENTIFIER;
	} else if (Music *mus = unsmob_music (sid)) {
		mus = mus->clone ();
		*destination = mus->self_scm ();
		unsmob_music (*destination)->
			set_property ("origin", make_input (last_input_));
		return dynamic_cast<Event*> (mus)
			? EVENT_IDENTIFIER : MUSIC_IDENTIFIER;
	} else if (unsmob_duration (sid)) {
		*destination = unsmob_duration (sid)->smobbed_copy ();
		return DURATION_IDENTIFIER;
	} else if (unsmob_output_def (sid)) {
		Output_def *p = unsmob_output_def (sid);
		p = p->clone ();

		*destination = p->self_scm ();
		return OUTPUT_DEF_IDENTIFIER;
	} else if (Text_interface::markup_p (sid)) {
		*destination = sid;
		return MARKUP_IDENTIFIER;
	}

	return -1;	
}

Music *
property_op_to_music (SCM op)
{
	Music *m = 0;
	SCM tag = scm_car (op);
	SCM symbol = scm_cadr (op);
	SCM args = scm_cddr (op);
	SCM grob_val = SCM_UNDEFINED;
	SCM grob_sym = SCM_UNDEFINED;
	SCM val = SCM_UNDEFINED;
	
	if (tag == ly_symbol2scm ("assign"))
		{
		m = MY_MAKE_MUSIC ("PropertySet");
		val = scm_car (args);
		}
	else if (tag == ly_symbol2scm ("unset"))
		m = MY_MAKE_MUSIC ("PropertyUnset");
	else if (tag == ly_symbol2scm ("poppush")
		 || tag == ly_symbol2scm ("push"))
		{
		m = MY_MAKE_MUSIC ("OverrideProperty");
		grob_sym = scm_car (args);
		grob_val = scm_cadr (args);
		}
	else if (tag == ly_symbol2scm ("pop")) {
		m = MY_MAKE_MUSIC ("RevertProperty");
		grob_sym = scm_car (args);
		}

	m->set_property ("symbol", symbol);

	if (val != SCM_UNDEFINED)
		m->set_property ("value", val);
	if (grob_val != SCM_UNDEFINED)
		m->set_property ("grob-value", grob_val);

	if (grob_sym != SCM_UNDEFINED)
		{
		bool itc = internal_type_checking_global_b;
		/* UGH.
		*/
		bool autobeam = ly_c_equal_p (symbol, ly_symbol2scm ("autoBeamSettings"));
		if (autobeam)
			internal_type_checking_global_b = false;
		m->set_property ("grob-property", grob_sym);
		if (autobeam)
			internal_type_checking_global_b = itc;
		}

	if (tag == ly_symbol2scm ("poppush"))
		m->set_property ("pop-first", SCM_BOOL_T);


	return m;
}

Music*
context_spec_music (SCM type, SCM id, Music *m, SCM ops)
{
	Music *csm = MY_MAKE_MUSIC ("ContextSpeccedMusic");

	csm->set_property ("element", m->self_scm ());
	scm_gc_unprotect_object (m->self_scm ());

	csm->set_property ("context-type",
		scm_is_symbol (type) ? type : scm_string_to_symbol (type));
	csm->set_property ("property-operations", ops);

	if (scm_is_string (id))
		csm->set_property ("context-id", id);
	return csm;
}

/*
FIXME: this should be postponed until the music hits \Score
*/
SCM
get_next_unique_context ()
{
	static int new_context_count;
	char s[1024];
	snprintf (s, 1024, "uniqueContext%d", new_context_count++);
	return scm_makfrom0str (s);
}

