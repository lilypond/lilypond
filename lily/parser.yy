%{ // -*-Fundamental-*-

/*
  parser.yy -- Bison/C++ parser for lilypond

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  Two shift/reduce problems:
    -
    -
 */

/*

the rules for who is protecting what are very shady. TODO: uniformise
this.


*/

#include <ctype.h>

#include "translator-def.hh"
#include "lily-guile.hh"
#include "misc.hh"
#include "my-lily-lexer.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "main.hh"
#include "file-path.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "my-lily-parser.hh"
#include "score.hh"
#include "input-file-results.hh"
#include "input.hh"
#include "lilypond-input-version.hh"
#include "scm-hash.hh"
#include "auto-change-iterator.hh"
#include "chord.hh"
#include "ly-modules.hh"
#include "music-sequence.hh"
#include "input-smob.hh"
#include "event.hh"

bool
regular_identifier_b (SCM id)
{
  String str = ly_scm2string (id);
  char const *s = str.to_str0 () ;

  bool v = true;
  while (*s && v)
   {
        v = v && isalpha (*s);
        s++;
   }
  return v;
}



bool
is_duration_b (int t)
{
  return t && t == 1 << intlog2 (t);
}

void
set_music_properties (Music *p, SCM a)
{
  for (SCM k = a; gh_pair_p (k); k = ly_cdr (k))
	{
	p->internal_set_mus_property (ly_caar (k), ly_cdar (k));
	}
}



#define MY_MAKE_MUSIC(x)  make_music_by_name (ly_symbol2scm (x))

Music* 
set_property_music (SCM sym, SCM value)
{
	Music * p = MY_MAKE_MUSIC("PropertySet");
	p->set_mus_property ("symbol", sym);
	p->set_mus_property ("value", value);
	return p;
}


// needed for bison.simple's malloc () and free ()

// #include <malloc.h>
#include <stdlib.h>



#define YYERROR_VERBOSE 1

My_lily_parser* my_lily_parser;
#define YYPARSE_PARAM my_lily_parser
#define YYLEX_PARAM my_lily_parser
#define THIS\
	((My_lily_parser *) my_lily_parser)

#define yyerror THIS->parser_error

%}

/* We use SCMs to do strings, because it saves us the trouble of
deleting them.  Let's hope that a stack overflow doesnt trigger a move
of the parse stack onto the heap. */


%union {

    Link_array<Music> *reqvec;

    String *string; // needed by the lexer as temporary scratch area.
    Music *music;
    Score *score;
    Music_output_def * outputdef;
    SCM scm;
    int i;
}
%{

int
yylex (YYSTYPE *s,  void * v)
{
	My_lily_parser	 *pars = (My_lily_parser*) v;
	My_lily_lexer * lex = pars->lexer_;

	lex->lexval = (void*) s;
	lex->prepare_for_next_token();
	return lex->yylex ();
}


%}

%pure_parser

/* tokens which are not keywords */
%token AUTOCHANGE
%token ALIAS
%token APPLY
%token ACCEPTS
%token ALTERNATIVE
%token BAR
%token BREATHE
%token CHORDMODIFIERS
%token CHORDS
%token CHAR_T
%token CLEF
%token CM_T
%token CONSISTS
%token DURATION
%token SEQUENTIAL
%token GROBDESCRIPTIONS
%token SIMULTANEOUS
%token CONSISTSEND
%token DENIES
%token EXTENDER
%token FIGURES FIGURE_OPEN FIGURE_CLOSE
%token FIGURE_BRACKET_CLOSE FIGURE_BRACKET_OPEN
%token GRACE 
%token HEADER
%token HYPHEN
%token IN_T
%token INVALID
%token KEY
%token LYRICS
%token MARK
%token MULTI_MEASURE_REST
%token MIDI
%token MM_T
%token PITCH
%token DEFAULT
%token NAME
%token PITCHNAMES
%token NOTES
%token ONCE
%token PAPER
%token PARTIAL
%token PROPERTY
%token OVERRIDE SET REVERT 
%token PT_T
%token RELATIVE
%token REMOVE
%token REPEAT
%token ADDLYRICS
%token PARTCOMBINE
%token SCM_T
%token SCORE
%token SKIP
%token SPANREQUEST
%token COMMANDSPANREQUEST
%token TEMPO
%token OUTPUTPROPERTY
%token TIME_T
%token TIMES
%token TRANSLATOR
%token TRANSPOSE
%token TYPE
%token UNSET
%token CONTEXT
%token REST

/* escaped */
%token E_CHAR E_EXCLAMATION E_SMALLER E_BIGGER E_OPEN E_CLOSE
%token E_LEFTSQUARE E_RIGHTSQUARE E_TILDE
%token E_BACKSLASH
%token <i> E_UNSIGNED
%token CHORD_BASS CHORD_COLON CHORD_MINUS CHORD_CARET  CHORD_SLASH
%token FIGURE_SPACE

%type <i>	exclamations questions dots optional_rest
%type <i>  	bass_number bass_mod
%type <scm> 	br_bass_figure bass_figure figure_list figure_spec
%token <i>	DIGIT
%token <scm>	NOTENAME_PITCH
%token <scm>	TONICNAME_PITCH
%token <scm>	CHORDMODIFIER_PITCH
%token <scm>	DURATION_IDENTIFIER
%token <scm>    FRACTION
%token <id>	IDENTIFIER


%token <scm>	SCORE_IDENTIFIER
%token <scm>	MUSIC_OUTPUT_DEF_IDENTIFIER
%token <scm>	NUMBER_IDENTIFIER
%token <scm>	EVENT_IDENTIFIER
%token <scm>	MUSIC_IDENTIFIER TRANSLATOR_IDENTIFIER
%token <scm>	STRING_IDENTIFIER SCM_IDENTIFIER 
%token <scm>	RESTNAME
%token <scm>	STRING   
%token <scm>	SCM_T
%token <i>	UNSIGNED
%token <scm>   REAL

%type <outputdef> output_def
%type <scm> 	lilypond_header lilypond_header_body
%type <music>	open_event_parens close_event_parens open_event close_event
%type <music> event_with_dir event_that_take_dir verbose_event
%type <i>	sub_quotes sup_quotes
%type <music>	simple_element  event_chord command_element Simple_music  Composite_music 
%type <music>	Repeated_music
%type <scm>     Alternative_music
%type <i>	tremolo_type
%type <i>	bare_int  bare_unsigned
%type <i>	script_dir
%type <scm>	identifier_init 

%type <scm> steno_duration optional_notemode_duration multiplied_duration
%type <scm>  verbose_duration
	
%type <reqvec>  pre_events post_events
%type <music> gen_text_def
%type <scm>   steno_pitch pitch absolute_pitch
%type <scm>   explicit_pitch steno_tonic_pitch

%type <scm>	chord_additions chord_subtractions chord_notes chord_step
%type <music>	chord
%type <scm>	chord_note chord_inversion chord_bass
%type <scm>	duration_length fraction

%type <scm>  embedded_scm scalar
%type <music>	Music Sequential_music Simultaneous_music 
%type <music>	relative_music re_rhythmed_music part_combined_music
%type <music>	property_def translator_change  simple_property_def
%type <scm> Music_list
%type <outputdef>  music_output_def_body
%type <music> shorthand_command_req
%type <music>	post_event 
%type <music> command_req verbose_command_req
%type <music>	extender_req
%type <music> hyphen_req
%type <music> string_event
%type <scm>	string bare_number number_expression number_term number_factor 

%type <score>	score_block score_body

%type <scm>	translator_spec_block translator_spec_body
%type <music> 	tempo_event
%type <scm> notenames_body notenames_block chordmodifiers_block
%type <scm>	script_abbreviation



%left '-' '+'

/* We don't assign precedence to / and *, because we might need varied
prec levels in different prods */

%left UNARY_MINUS

%%

lilypond:	/* empty */
	| lilypond toplevel_expression {}
	| lilypond assignment  { }
	| lilypond error {
		THIS->error_level_  = 1;
	}
	| lilypond INVALID	{
		THIS->error_level_  = 1;
	}
	;

toplevel_expression:
	notenames_block			{
		THIS->lexer_->pitchname_tab_ =  $1;
	}
	| chordmodifiers_block			{
		THIS->lexer_->chordmodifier_tab_  = $1;
	}
	| lilypond_header {
		THIS->input_file_->header_ = $1;
	}
	| score_block {
		THIS->input_file_->scores_.push ($1);
	}
	| output_def {
		if (dynamic_cast<Paper_def*> ($1))
			THIS->lexer_->set_identifier (scm_makfrom0str ("$defaultpaper"), $1->self_scm ());
		else if (dynamic_cast<Midi_def*> ($1))
			THIS->lexer_->set_identifier (scm_makfrom0str ("$defaultmidi"), $1->self_scm ());
	}
	;

embedded_scm:
	SCM_T
	| SCM_IDENTIFIER 
	;


chordmodifiers_block:
	CHORDMODIFIERS notenames_body   {  $$ = $2; }
	;

notenames_block:
	PITCHNAMES notenames_body   {  $$ = $2; }
	;

notenames_body:
	embedded_scm	{
	  int i = scm_ilength ($1);

	  SCM tab = scm_make_vector (gh_int2scm (i), SCM_EOL);
	  for (SCM s = $1; gh_pair_p (s); s = ly_cdr (s)) {
		SCM pt = ly_cdar (s);
		if (!unsmob_pitch (pt))
			THIS->parser_error ("Need pitch object.");
		else
			scm_hashq_set_x (tab, ly_caar (s), pt);
	  }

	  $$ = tab;
	}
	;

lilypond_header_body:
	{
		$$ = ly_make_anonymous_module (); 
		THIS->lexer_->add_scope ($$);
	}
	| lilypond_header_body assignment  { 
		
	}
	;

lilypond_header:
	HEADER '{' lilypond_header_body '}'	{
		$$ = THIS->lexer_-> remove_scope();
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

		if (! regular_identifier_b ($1))
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
	| translator_spec_block {
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
	| verbose_duration {
		$$ = $1;
	}
	| number_expression {
		$$ = $1;
	}
	| string {
		$$ = $1;
	}
	| embedded_scm	{
		$$ = $1;
	}
	;

translator_spec_block:
	TRANSLATOR '{' translator_spec_body '}'
		{
		$$ = $3;
	}
	;

translator_spec_body:
	TRANSLATOR_IDENTIFIER	{
		$$ = $1;
		unsmob_translator_def ($$)-> set_spot (THIS->here_input ());
	}
	| TYPE STRING 	{
		$$ = Translator_def::make_scm ();
		Translator_def*td =  unsmob_translator_def ($$);
		td->translator_group_type_ = $2;
		td->set_spot (THIS->here_input ());
	}
	| translator_spec_body STRING '=' embedded_scm			{
		unsmob_translator_def ($$)->add_property_assign ($2, $4);
	}
	| translator_spec_body STRING OVERRIDE embedded_scm '=' embedded_scm {
		unsmob_translator_def ($$)
			->add_push_property (scm_string_to_symbol ($2), $4, $6);
	}
	| translator_spec_body STRING SET embedded_scm '=' embedded_scm {
		unsmob_translator_def ($$)
			->add_push_property (scm_string_to_symbol ($2), $4, $6);
	}
	| translator_spec_body STRING REVERT embedded_scm  {
	  unsmob_translator_def ($$)->add_pop_property (
		scm_string_to_symbol ($2), $4);
	}
	| translator_spec_body NAME STRING  {
		unsmob_translator_def ($$)->type_name_ = $3;
	}
	| translator_spec_body CONSISTS STRING  {
		unsmob_translator_def ($$)->add_element ($3);
	}
	| translator_spec_body ALIAS STRING  {
		Translator_def*td = unsmob_translator_def ($$);
		td->type_aliases_ = scm_cons ($3, td->type_aliases_);
	}
	| translator_spec_body GROBDESCRIPTIONS embedded_scm {
		Translator_def*td = unsmob_translator_def($$);
		// td->add_property_assign (ly_symbol2scm ("allGrobDescriptions"), $3);
		for (SCM p = $3; gh_pair_p (p); p = ly_cdr (p))
			td->add_property_assign (scm_symbol_to_string (ly_caar (p)), ly_cdar (p));
	}
	| translator_spec_body CONSISTSEND STRING  {
		unsmob_translator_def ($$)->add_last_element ( $3);
	}
	| translator_spec_body ACCEPTS STRING  {
		unsmob_translator_def ($$)->set_acceptor ($3,true);
	}
	| translator_spec_body DENIES STRING  {
		unsmob_translator_def ($$)->set_acceptor ($3,false);
	}
	| translator_spec_body REMOVE STRING  {
		unsmob_translator_def ($$)->remove_element ($3);
	}
	;

/*
	SCORE
*/
score_block:
	SCORE { 
		THIS->push_spot ();
	}
	/*cont*/ '{' score_body '}' 	{
		THIS->pop_spot ();
		$$ = $4;
		if (!$$->defs_.size ())
		{
		  Music_output_def *id =
			unsmob_music_output_def (THIS->lexer_->lookup_identifier ("$defaultpaper"));
		  $$->add_output (id ? id->clone () :  new Paper_def );
		}
	}
	;

score_body:
	Music	{
		$$ = new Score;
	
		$$->set_spot (THIS->here_input ());
		SCM m = $1->self_scm ();
		scm_gc_unprotect_object (m);

		/*
			guh.
		*/
		SCM check_funcs = scm_c_eval_string ("toplevel-music-functions");
		for (; gh_pair_p (check_funcs); check_funcs = gh_cdr (check_funcs))
			m = gh_call1 (gh_car (check_funcs), m);
		$$->music_ = m;

	}
	| SCORE_IDENTIFIER {
		$$ = unsmob_score ($1);
		$$->set_spot (THIS->here_input ());
	}
	| score_body lilypond_header 	{
		$$->header_ = $2;
	}
	| score_body output_def {
		$$->add_output ($2);
	}
	| score_body error {

	}
	;


/*
	MIDI
*/
output_def:
	music_output_def_body '}' {
		$$ = $1;
		THIS-> lexer_-> remove_scope ();
	}
	;

music_output_def_body:
	MIDI '{'    {
		Music_output_def *id = unsmob_music_output_def (THIS->lexer_->lookup_identifier ("$defaultmidi"));


		Midi_def* p =0;
		if (id)
			p = dynamic_cast<Midi_def*> (id->clone ());
		else
			p = new Midi_def;

		$$ = p;
		THIS->lexer_->add_scope (p->scope_);
	}
	| PAPER '{' 	{
		Music_output_def *id = unsmob_music_output_def (THIS->lexer_->lookup_identifier ("$defaultpaper"));
		  Paper_def *p = 0;
		if (id)
			p = dynamic_cast<Paper_def*> (id->clone ());
		else
			p = new Paper_def;

		THIS->lexer_->add_scope (p->scope_);
		$$ = p;
	}
	| PAPER '{' MUSIC_OUTPUT_DEF_IDENTIFIER 	{
		Music_output_def * o =  unsmob_music_output_def ($3);
		$$ =o;

		THIS->lexer_->add_scope (o->scope_);
	}
	| MIDI '{' MUSIC_OUTPUT_DEF_IDENTIFIER 	{
		Music_output_def * o =  unsmob_music_output_def ($3);
		$$ = o;

		THIS->lexer_->add_scope (o->scope_);
	}
	| music_output_def_body assignment  {

	}
	| music_output_def_body translator_spec_block	{
		$$->assign_translator ($2);
	}
	| music_output_def_body tempo_event  {
		/*
			junk this ? there already is tempo stuff in
			music.
		*/
		int m = gh_scm2int ( $2->get_mus_property ("metronome-count"));
		Duration *d = unsmob_duration ($2->get_mus_property ("duration"));
		Midi_def * md = dynamic_cast<Midi_def*> ($$);
		if (md)
			md->set_tempo (d->get_length (), m);
	}
	| music_output_def_body error {

	}
	;

tempo_event:
	TEMPO steno_duration '=' bare_unsigned	{
		$$ = MY_MAKE_MUSIC("TempoEvent");
		$$->set_mus_property ("duration", $2);
		$$->set_mus_property ("metronome-count", gh_int2scm ( $4));
	}
	;

/*
The representation of a  list is the

  (LIST . LAST-CONS)

 to have  efficient append.
*/
Music_list: /* empty */ {
		$$ = scm_cons (SCM_EOL, SCM_EOL);
	}
	| Music_list Music {
		SCM s = $$;
		SCM c = scm_cons ($2->self_scm (), SCM_EOL);
		scm_gc_unprotect_object ($2->self_scm ()); /* UGH */
		if (gh_pair_p (ly_cdr (s)))
			gh_set_cdr_x (ly_cdr (s), c); /* append */
		else
			gh_set_car_x (s, c); /* set first cons */
		gh_set_cdr_x (s, c) ;  /* remember last cell */ 
	}
	| Music_list error {
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
	REPEAT string bare_unsigned Music Alternative_music
	{
		Music *beg = $4;
		int times = $3;
		SCM alts = gh_pair_p ($5) ? gh_car ($5) : SCM_EOL;
		if (times < scm_ilength (alts)) {
		  unsmob_music (gh_car (alts))
		    ->origin ()->warning (
		    _("More alternatives than repeats.  Junking excess alternatives."));
		  alts = ly_truncate_list (times, alts);
		}


		static SCM proc;
		if (!proc)
			proc = scm_c_eval_string ("make-repeated-music");

		SCM mus = scm_call_1 (proc, $2);
		scm_gc_protect_object (mus); // UGH. 
		Music *r =unsmob_music (mus);
		if (beg)
			{
			r-> set_mus_property ("element", beg->self_scm ());
			scm_gc_unprotect_object (beg->self_scm ());
			}
		r->set_mus_property ("repeat-count", gh_int2scm (times >? 1));

		r-> set_mus_property ("elements",alts);
		if (gh_equal_p ($2, scm_makfrom0str ("tremolo")))
		{
		/*
		we can not get durations and other stuff correct down the line, so we have to
		add to the duration log here.
		*/
			SCM func = scm_primitive_eval (ly_symbol2scm ("shift-duration-log"));
			if (($3 % 3) == 0)
			  gh_call3 (func, r->self_scm (), gh_int2scm(-intlog2 ($3*2/3)),gh_int2scm(1));
			else
			  gh_call3 (func, r->self_scm (), gh_int2scm(-intlog2 ($3)), gh_int2scm(0));
		}
		r->set_spot (*$4->origin ());

		$$ = r;
	}
	;

Sequential_music:
	SEQUENTIAL '{' Music_list '}'		{
		$$ = MY_MAKE_MUSIC("SequentialMusic");
		$$->set_mus_property ("elements", ly_car ($3));
		$$->set_spot(THIS->here_input());
	}
	| '{' Music_list '}'		{
		$$ = MY_MAKE_MUSIC("SequentialMusic");
		$$->set_mus_property ("elements", ly_car ($2));
		$$->set_spot(THIS->here_input());
	}
	;

Simultaneous_music:
	SIMULTANEOUS '{' Music_list '}'{
		$$ = MY_MAKE_MUSIC("SimultaneousMusic");
		$$->set_mus_property ("elements", ly_car ($3));
		$$->set_spot(THIS->here_input());

	}
	| '<' Music_list '>'	{
		$$ = MY_MAKE_MUSIC("SimultaneousMusic");
		$$->set_mus_property ("elements", ly_car ($2));
		$$->set_spot(THIS->here_input());
	}
	;

Simple_music:
	event_chord		{ $$ = $1; }
	| OUTPUTPROPERTY embedded_scm embedded_scm '=' embedded_scm	{
		SCM pred = $2;
		if (!gh_symbol_p ($3))
		{
			THIS->parser_error (_ ("Second argument must be a symbol")); 
		}
		/* Should check # args */
		if (!gh_procedure_p (pred))
		{
			THIS->parser_error (_ ("First argument must be a procedure taking one argument"));
		}

		Music*m = MY_MAKE_MUSIC("OutputPropertySetMusic");
		m->set_mus_property ("predicate", pred);
		m->set_mus_property ("grob-property", $3);
		m->set_mus_property ("grob-value",  $5);

		$$ = m;
	}
	| MUSIC_IDENTIFIER {
		$$ = unsmob_music ($1);
	}
	| property_def
	| translator_change
	;


Composite_music:
	CONTEXT STRING Music	{
		Music*csm =MY_MAKE_MUSIC("ContextSpeccedMusic");

		csm->set_mus_property ("element", $3->self_scm ());
		scm_gc_unprotect_object ($3->self_scm ());

		csm->set_mus_property ("context-type",$2);
		csm->set_mus_property ("context-id", scm_makfrom0str (""));

		$$ = csm;
	}
	| AUTOCHANGE STRING Music	{
	Music*chm = MY_MAKE_MUSIC("AutoChangeMusic");
		chm->set_mus_property ("element", $3->self_scm ());
		chm->set_mus_property ("iterator-ctor", Auto_change_iterator::constructor_proc);

		scm_gc_unprotect_object ($3->self_scm ());
		chm->set_mus_property ("what", $2); 

		$$ = chm;
		chm->set_spot (*$3->origin ());
	}
	| GRACE Music {
#if 1
	/*
		The other version is for easier debugging  of
		Sequential_music_iterator in combination with grace notes.
	*/

		SCM start = THIS->lexer_->lookup_identifier ("startGraceMusic");
		SCM stop = THIS->lexer_->lookup_identifier ("stopGraceMusic");
		Music *startm = unsmob_music (start);
		Music *stopm = unsmob_music (stop);

		SCM ms = SCM_EOL;
		if (stopm) {
			stopm = stopm->clone ();
			ms = scm_cons (stopm->self_scm (), ms);
			scm_gc_unprotect_object (stopm->self_scm ());
		}
		ms = scm_cons ($2->self_scm (), ms);
		scm_gc_unprotect_object ($2->self_scm());
		if (startm) {
			startm = startm->clone ();
			ms = scm_cons (startm->self_scm () , ms);
			scm_gc_unprotect_object (startm->self_scm ());
		}

	
		Music* seq = MY_MAKE_MUSIC("SequentialMusic");
		seq->set_mus_property ("elements", ms);

		
		$$ = MY_MAKE_MUSIC("GraceMusic");
		$$->set_mus_property ("element", seq->self_scm ());
		scm_gc_unprotect_object (seq->self_scm ());
#else
		$$ = MY_MAKE_MUSIC("GraceMusic");
		$$->set_mus_property ("element", $2->self_scm ());
		scm_gc_unprotect_object ($2->self_scm ());
#endif
	}
	| CONTEXT string '=' string Music {
		Music * csm = MY_MAKE_MUSIC("ContextSpeccedMusic");

		csm->set_mus_property ("element", $5->self_scm ());
		scm_gc_unprotect_object ($5->self_scm ());

		csm->set_mus_property ("context-type", $2);
		csm->set_mus_property ("context-id", $4);

		$$ = csm;
	}
	| TIMES {
		THIS->push_spot ();
	}
	/* CONTINUED */ 
		fraction Music 	

	{
		int n = gh_scm2int (ly_car ($3)); int d = gh_scm2int (ly_cdr ($3));
		Music *mp = $4;
	$$= MY_MAKE_MUSIC("TimeScaledMusic");
		$$->set_spot (THIS->pop_spot ());


		$$->set_mus_property ("element", mp->self_scm ());
		scm_gc_unprotect_object (mp->self_scm ());
		$$->set_mus_property ("numerator", gh_int2scm (n));
		$$->set_mus_property ("denominator", gh_int2scm (d));
		$$->compress (Moment (Rational (n,d)));

	}
	| Repeated_music		{ $$ = $1; }
	| Simultaneous_music		{ $$ = $1; }
	| Sequential_music		{ $$ = $1; }
	| TRANSPOSE pitch Music {
		$$ = MY_MAKE_MUSIC("TransposedMusic");
		Music *p = $3;
		Pitch pit = *unsmob_pitch ($2);

		p->transpose (pit);
		$$->set_mus_property ("element", p->self_scm ());
		scm_gc_unprotect_object (p->self_scm ());
	}
	| TRANSPOSE steno_tonic_pitch Music {
		$$ = MY_MAKE_MUSIC("TransposedMusic");
		Music *p = $3;
		Pitch pit = *unsmob_pitch ($2);

		p->transpose (pit);
		$$->set_mus_property ("element", p->self_scm ());
		scm_gc_unprotect_object (p->self_scm ());
	
	}
	| APPLY embedded_scm Music  {
		SCM ret = gh_call1 ($2, $3->self_scm ());
		Music *m = unsmob_music (ret);
		if (!m) {
			THIS->parser_error ("\\apply must return a Music");
			m = MY_MAKE_MUSIC("Music");
			}
		$$ = m;
	}
	| NOTES
		{ THIS->lexer_->push_note_state (); }
	Music
		{ $$ = $3;
		  THIS->lexer_->pop_state ();
		}
	| FIGURES
		{ THIS->lexer_->push_figuredbass_state (); }
	Music
		{
		  Music * chm = MY_MAKE_MUSIC("UntransposableMusic");
		  chm->set_mus_property ("element", $3->self_scm ());
		  $$ = chm;
		  scm_gc_unprotect_object ($3->self_scm());

		  THIS->lexer_->pop_state ();
	}
	| CHORDS
		{ THIS->lexer_->push_chord_state (); }
	Music
		{
		  Music * chm = MY_MAKE_MUSIC("UnrelativableMusic");
		  chm->set_mus_property ("element", $3->self_scm ());
		  scm_gc_unprotect_object ($3->self_scm());
		  $$ = chm;

		  THIS->lexer_->pop_state ();
	}
	| LYRICS
		{ THIS->lexer_->push_lyric_state (); }
	Music
		{
		  $$ = $3;
		  THIS->lexer_->pop_state ();
	}
	| relative_music	{ $$ = $1; }
	| re_rhythmed_music	{ $$ = $1; } 
	| part_combined_music	{ $$ = $1; } 
	;

relative_music:
	RELATIVE absolute_pitch Music {
		Music * p = $3;
		Pitch pit = *unsmob_pitch ($2);
		$$ = MY_MAKE_MUSIC("RelativeOctaveMusic");

		$$->set_mus_property ("element", p->self_scm ());
		scm_gc_unprotect_object (p->self_scm ());

		$$->set_mus_property ("last-pitch", p->to_relative_octave (pit).smobbed_copy ());

	}
	;

re_rhythmed_music:
	ADDLYRICS Music Music {
	Music*l =MY_MAKE_MUSIC("LyricCombineMusic");
	  l->set_mus_property ("elements", gh_list ($2->self_scm (), $3->self_scm (), SCM_UNDEFINED));
	  scm_gc_unprotect_object ($3->self_scm ());
	  scm_gc_unprotect_object ($2->self_scm ());
	  $$ = l;
	}
	;

part_combined_music:
	PARTCOMBINE STRING Music Music {
	Music * p= MY_MAKE_MUSIC("PartCombineMusic");
		p->set_mus_property ("what", $2);
		p->set_mus_property ("elements", gh_list ($3->self_scm (),$4->self_scm (), SCM_UNDEFINED));  

		scm_gc_unprotect_object ($3->self_scm ());
		scm_gc_unprotect_object ($4->self_scm ());  

		$$ = p;
	}
	;

translator_change:
	TRANSLATOR STRING '=' STRING  {
		Music*t= MY_MAKE_MUSIC("TranslatorChange");
		t-> set_mus_property ("change-to-type", $2);
		t-> set_mus_property ("change-to-id", $4);

		$$ = t;
		$$->set_spot (THIS->here_input ());
	}
	;

property_def:
	simple_property_def
	| ONCE simple_property_def {
		$$ = $2;
		SCM e = $2->get_mus_property ("element");
		unsmob_music (e)->set_mus_property ("once", SCM_BOOL_T);
	}
	;

simple_property_def:
	PROPERTY STRING '.' STRING '='  scalar {
		Music *t = set_property_music (scm_string_to_symbol ($4), $6);
		Music *csm = MY_MAKE_MUSIC("ContextSpeccedMusic");

 		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING UNSET {
		
		Music *t = MY_MAKE_MUSIC("PropertyUnset");
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));

		Music *csm = MY_MAKE_MUSIC("ContextSpeccedMusic");
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING SET embedded_scm '=' embedded_scm {
		bool autobeam
		  = gh_equal_p ($4, scm_makfrom0str ("autoBeamSettings"));
		bool itc = internal_type_checking_global_b;
		Music *t = MY_MAKE_MUSIC("OverrideProperty");
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));
		t->set_mus_property ("pop-first", SCM_BOOL_T);
		if (autobeam)
			internal_type_checking_global_b = false;
		t->set_mus_property ("grob-property", $6);
		if (autobeam)
			internal_type_checking_global_b = itc;
		t->set_mus_property ("grob-value", $8);

		Music *csm = MY_MAKE_MUSIC("ContextSpeccedMusic");
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());
		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING OVERRIDE
		embedded_scm '=' embedded_scm
	{
		/*
			UGH UGH UGH UGH.
		*/
		bool autobeam
		  = gh_equal_p ($4, scm_makfrom0str ("autoBeamSettings"));
		bool itc = internal_type_checking_global_b;

		Music *t = MY_MAKE_MUSIC("OverrideProperty");
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));
		if (autobeam)
			internal_type_checking_global_b = false;
		t->set_mus_property ("grob-property", $6);
		t->set_mus_property ("grob-value", $8);
		if (autobeam)
			internal_type_checking_global_b = itc;

		Music *csm = MY_MAKE_MUSIC("ContextSpeccedMusic");
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);

	}
	| PROPERTY STRING '.' STRING REVERT embedded_scm {
		Music *t = MY_MAKE_MUSIC("RevertProperty");
		bool autobeam
		  = gh_equal_p ($4, scm_makfrom0str ("autoBeamSettings"));
		bool itc = internal_type_checking_global_b;

		t->set_mus_property ("symbol", scm_string_to_symbol ($4));
		if (autobeam)
			internal_type_checking_global_b = false;
		t->set_mus_property ("grob-property", $6);
		if (autobeam)
			internal_type_checking_global_b = itc;
	
		Music *csm = MY_MAKE_MUSIC("ContextSpeccedMusic");
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	;


scalar:
        string          { $$ = $1; }
        | bare_int      { $$ = gh_int2scm ($1); }
        | embedded_scm  { $$ = $1; }
        ;


event_chord:
	pre_events {
		THIS->push_spot ();
	} /*cont */ simple_element post_events	{
		Music_sequence *l = dynamic_cast<Music_sequence*> ($3);
		
		$1->concat (*$4);
	        for (int i=0; i < $1->size (); i++) {
		  Music * m = $1->elem (i);
		  l->append_music (m);
		}
 		$$ = $3;

		delete $1;
		delete $4;
	}
	| command_element
	;

command_element:
	command_req {
		$$ = MY_MAKE_MUSIC("EventChord");
		$$->set_mus_property ("elements", scm_cons ($1->self_scm (), SCM_EOL));
  	  scm_gc_unprotect_object ($1->self_scm());

		$$-> set_spot (THIS->here_input ());
		$1-> set_spot (THIS->here_input ());
	}
	| E_LEFTSQUARE {
		Music *l = MY_MAKE_MUSIC("LigatureEvent");
		l->set_mus_property ("span-direction", gh_int2scm (START));
		l->set_spot (THIS->here_input ());

		$$ = MY_MAKE_MUSIC("EventChord");
		$$->set_mus_property ("elements", scm_cons (l->self_scm (), SCM_EOL));
  	  scm_gc_unprotect_object (l->self_scm());
		$$->set_spot (THIS->here_input ());
	}
	| E_RIGHTSQUARE {
		Music *l = MY_MAKE_MUSIC("LigatureEvent");
		l->set_mus_property ("span-direction", gh_int2scm (STOP));
		l->set_spot (THIS->here_input ());

		$$ = MY_MAKE_MUSIC("EventChord");
		$$->set_mus_property ("elements", scm_cons (l->self_scm (), SCM_EOL));
		$$->set_spot (THIS->here_input ());
	  scm_gc_unprotect_object (l->self_scm());

	}
	| E_BACKSLASH {
		$$ = MY_MAKE_MUSIC("VoiceSeparator");
		$$->set_spot (THIS->here_input ());
	}
	| '|'      {

		$$ = MY_MAKE_MUSIC("BarCheck");
		$$->set_spot (THIS->here_input ());
	}
	| BAR STRING  			{
		Music *t = set_property_music (ly_symbol2scm ("whichBar"), $2);

		Music *csm = MY_MAKE_MUSIC("ContextSpeccedMusic");
		csm->set_mus_property ("element", t->self_scm ());
		scm_gc_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm->set_mus_property ("context-type", scm_makfrom0str ("Timing"));
	}
	| PARTIAL duration_length  	{
		Moment m = - unsmob_duration ($2)->get_length ();
		Music * p = set_property_music (ly_symbol2scm ( "measurePosition"),m.smobbed_copy ());

		Music * sp = MY_MAKE_MUSIC("ContextSpeccedMusic");
		sp->set_mus_property ("element", p->self_scm ());
		scm_gc_unprotect_object (p->self_scm ());

		$$ =sp ;
		sp-> set_mus_property ("context-type", scm_makfrom0str ("Timing"));
	}
	| CLEF STRING  {
		static SCM proc ;
		if (!proc)
			proc = scm_c_eval_string ("make-clef-set");

		SCM result = scm_call_1 (proc, $2);
		scm_gc_protect_object (result);
		$$ = unsmob_music (result);
	}
	| TIME_T fraction  {
		Music * p1 = set_property_music (ly_symbol2scm ( "timeSignatureFraction"), $2);

		int l = gh_scm2int (ly_car ($2));
		int o = gh_scm2int (ly_cdr ($2));

		Moment one_beat = Moment (1)/Moment (o);
		Moment len = Moment (l) * one_beat;


		Music *p2 = set_property_music (ly_symbol2scm ("measureLength"), len.smobbed_copy ());
		Music *p3 = set_property_music (ly_symbol2scm ("beatLength"), one_beat.smobbed_copy ());

		SCM list = scm_list_n (p1->self_scm (), p2->self_scm (), p3->self_scm(), SCM_UNDEFINED);
		Music *seq = MY_MAKE_MUSIC("SequentialMusic");
		seq->set_mus_property ("elements", list);
		

		Music * sp = MY_MAKE_MUSIC("ContextSpeccedMusic");
		sp->set_mus_property ("element", seq->self_scm ());

		scm_gc_unprotect_object (p3->self_scm ());
		scm_gc_unprotect_object (p2->self_scm ());
		scm_gc_unprotect_object (p1->self_scm ());
		scm_gc_unprotect_object (seq->self_scm ());

		$$ = sp;

		sp-> set_mus_property ("context-type", scm_makfrom0str ( "Timing"));
	}
	;

command_req:
	shorthand_command_req  	{ $$ = $1; }
	| verbose_command_req 	{ $$ = $1; }
	;

shorthand_command_req:
	extender_req {
		$$ = $1;
	}
	| hyphen_req {
		$$ = $1;
	}
	| '~'	{
		$$ = MY_MAKE_MUSIC("TieEvent");
	}
	| '['		{
		Music *b= MY_MAKE_MUSIC("BeamEvent");
		b->set_mus_property ("span-direction", gh_int2scm (START))
;
		$$ =b;


		THIS->last_beam_start_ = b->self_scm ();
	}
	| ']'		{
		Music *b= MY_MAKE_MUSIC("BeamEvent");
		b->set_mus_property ("span-direction", gh_int2scm (STOP));
		$$ = b;
	}
	| BREATHE {
		$$ = MY_MAKE_MUSIC("BreathingSignEvent");
	}
	| E_TILDE {
		$$ = MY_MAKE_MUSIC("PorrectusEvent");
	}
	;

verbose_command_req:
	MARK DEFAULT  {
		Music * m = MY_MAKE_MUSIC("MarkEvent");
		$$ = m;
	}
	| MARK scalar {
		Music *m = MY_MAKE_MUSIC("MarkEvent");
		m->set_mus_property ("label", $2);
		$$ = m;
	}
	| SKIP duration_length {
		Music * skip = MY_MAKE_MUSIC("SkipEvent");
		skip->set_mus_property ("duration", $2);

		$$ = skip;
	}
	| tempo_event {
		$$ = $1;
	}
	| KEY DEFAULT {
		Music *key= MY_MAKE_MUSIC("KeyChangeEvent");
		$$ = key;
	}
	| KEY NOTENAME_PITCH SCM_IDENTIFIER 	{
		Music *key= MY_MAKE_MUSIC("KeyChangeEvent");
		
		key->set_mus_property ("pitch-alist", $3);
 		((Music*)key)->transpose (* unsmob_pitch ($2));
		$$ = key; 
	}
	;

post_events:
	{
		$$ = new Link_array<Music>;
	}
	| post_events post_event {
		$2->set_spot (THIS->here_input ());
		$$->push ($2);
	}
	;

post_event:
	verbose_event
	| event_with_dir
	| close_event
	| string_event
	;

string_event:
	E_UNSIGNED {
		Music * s = MY_MAKE_MUSIC("StringNumberEvent");
		s->set_mus_property ("string-number",  gh_int2scm($1));
		s->set_spot (THIS->here_input ());
		$$ = s;
	}
	;


event_that_take_dir:
	gen_text_def
	| verbose_event
	| close_event
	| open_event
	| script_abbreviation {
		SCM s = THIS->lexer_->lookup_identifier ("dash" + ly_scm2string ($1));
		Music *a = MY_MAKE_MUSIC("ArticulationEvent");
		if (gh_string_p (s))
			a->set_mus_property ("articulation-type", s);
		else THIS->parser_error (_ ("Expecting string as script definition"));
		$$ = a;
	}
	;

event_with_dir:
	script_dir event_that_take_dir	{
		$2->set_mus_property ("direction", gh_int2scm ($1));
		$$ = $2;
	}
	;
	
verbose_event:
	EVENT_IDENTIFIER	{
		$$ = unsmob_music ($1);
	}
	| tremolo_type  {
               Music * a = MY_MAKE_MUSIC("TremoloEvent");
               a->set_spot (THIS->here_input ());
               a->set_mus_property ("tremolo-type", gh_int2scm ($1));
               $$ = a;
        }
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
		$$ ++ ;
	}
	;

steno_pitch:
	NOTENAME_PITCH	{
		$$ = $1;
	}
	| NOTENAME_PITCH sup_quotes 	{
		Pitch p = *unsmob_pitch ($1);
		p.octave_ +=  $2;
		$$ = p.smobbed_copy ();
	}
	| NOTENAME_PITCH sub_quotes	 {
		Pitch p =* unsmob_pitch ($1);

		p.octave_ +=  -$2;
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
		p.octave_ +=  $2;
		$$ = p.smobbed_copy ();
	}
	| TONICNAME_PITCH sub_quotes	 {
		Pitch p =* unsmob_pitch ($1);

		p.octave_ +=  -$2;
		$$ = p.smobbed_copy ();

	}
	;

pitch:
	steno_pitch {
		$$ = $1;
	}
	| explicit_pitch {
		$$ = $1;
	}
	;

explicit_pitch:
	PITCH embedded_scm {
		$$ = $2;
		if (!unsmob_pitch ($2)) {
			THIS->parser_error (_f ("Expecting musical-pitch value", 3));
			 $$ = Pitch ().smobbed_copy ();
		}
	}
	;

verbose_duration:
	DURATION embedded_scm 	{
		$$ = $2;
		if (!unsmob_duration ($2))
		{
			THIS->parser_error (_ ("Must have duration object"));
			$$ = Duration ().smobbed_copy ();
		}
	}
	;

extender_req:
	EXTENDER {
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = MY_MAKE_MUSIC("ExtenderEvent");
	}
	;

hyphen_req:
	HYPHEN {
		if (!THIS->lexer_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = MY_MAKE_MUSIC("HyphenEvent");
	}
	;

close_event:
	close_event_parens {
		$$ = $1;
		dynamic_cast<Music *> ($$)->set_mus_property ("span-direction", gh_int2scm (START))
;
	}
	;
 
close_event_parens:
	'('	{
		Music * s= MY_MAKE_MUSIC("SlurEvent");
		$$ = s;
		s->set_spot (THIS->here_input());
	}
	| E_OPEN	{
		Music * s= MY_MAKE_MUSIC("PhrasingSlurEvent");
		$$ = s;
		s->set_spot (THIS->here_input());
	}
	| E_SMALLER {
		Music *s =MY_MAKE_MUSIC("CrescendoEvent");
		$$ = s;
		s->set_spot (THIS->here_input());
	}
	| E_BIGGER {
		Music *s =MY_MAKE_MUSIC("DecrescendoEvent");
		$$ = s;
		s->set_spot (THIS->here_input());
	}
	;


open_event:
	open_event_parens {
		$$ = $1;
		dynamic_cast<Music *> ($$)->set_mus_property ("span-direction", gh_int2scm (STOP))
;
	}
	;

open_event_parens:
	E_EXCLAMATION 	{
		Music *s =  MY_MAKE_MUSIC("CrescendoEvent");
		s->set_spot (THIS->here_input());

		$$ = s;
	}
	| ')'	{
		Music * s= MY_MAKE_MUSIC("SlurEvent");
		$$ = s;
		s->set_spot (THIS->here_input());

	}
	| E_CLOSE	{
		Music * s= MY_MAKE_MUSIC("PhrasingSlurEvent");
		$$ = s;
		s->set_mus_property ("span-type", scm_makfrom0str ( "phrasing-slur"));
		s->set_spot (THIS->here_input());
	}
	;

gen_text_def:
	embedded_scm {
		Music *t = MY_MAKE_MUSIC("TextScriptEvent");
		t->set_mus_property ("text", $1);
		t->set_spot (THIS->here_input ());
		$$ = t;
	}
	| string {
		Music *t = MY_MAKE_MUSIC("TextScriptEvent");
		t->set_mus_property ("text", $1);
		t->set_spot (THIS->here_input ());
		$$ = t;
	}
	| DIGIT {
		String ds = to_string ($1);
		Music * t = MY_MAKE_MUSIC("TextScriptEvent");
		SCM finger = ly_symbol2scm ("finger");
		t->set_mus_property ("text",  scm_makfrom0str (ds.to_str0 ()));
		t->set_mus_property ("text-type" , finger);
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

pre_events:
	{
		$$ = new Link_array<Music>;
	}
	| pre_events open_event {
		$$->push ($2);
	}
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
	| verbose_duration {
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
	| verbose_duration {
		$$ = $1;
		THIS->default_duration_ = *unsmob_duration ($$);
	}	
	;

steno_duration:
	bare_unsigned dots		{
		int l = 0;
		if (!is_duration_b ($1))
			THIS->parser_error (_f ("not a duration: %d", $1));
		else
			l =  intlog2 ($1);

		$$ = Duration (l, $2).smobbed_copy ();
	}
	| DURATION_IDENTIFIER dots	{
		Duration *d =unsmob_duration ($1);
		Duration k (d->duration_log (),d->dot_count () + $2);

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
		Rational  m (gh_scm2int (ly_car ($3)), gh_scm2int (ly_cdr ($3)));

		$$ = unsmob_duration ($$)->compressed (m).smobbed_copy ();
	}
	;

fraction:
	FRACTION { $$ = $1; }
	| UNSIGNED '/' UNSIGNED {
		$$ = scm_cons (gh_int2scm ($1), gh_int2scm ($3));
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
		$$ =0;
	}
	| ':' bare_unsigned {
		if (!is_duration_b ($2))
			THIS->parser_error (_f ("not a duration: %d", $2));
		$$ = $2;
	}
	;


bass_number:
	DIGIT
	| UNSIGNED 
	;

bass_mod:
	'-' 	{ $$ = -1; }
	| '+'	{ $$ = 1; } 
	| '!'	{ $$ = 0; }
	;

bass_figure:
	FIGURE_SPACE {
		Music *bfr = MY_MAKE_MUSIC("BassFigureEvent");
		$$ = bfr->self_scm();
		scm_gc_unprotect_object ($$);
	}
	| bass_number  {
		Music *bfr = MY_MAKE_MUSIC("BassFigureEvent");
		$$ = bfr->self_scm();

		bfr->set_mus_property ("figure", gh_int2scm ($1));

		scm_gc_unprotect_object ($$);
	}
	| bass_figure bass_mod {
		Music *m = unsmob_music ($1);
		if ($2) {
			SCM salter =m->get_mus_property ("alteration");
			int alter = gh_number_p ( salter) ? gh_scm2int (salter) : 0;
			m->set_mus_property ("alteration",
				gh_int2scm (alter + $2));
		} else {
			m->set_mus_property ("alteration", gh_int2scm (0));
		}
	}
	;

br_bass_figure:
	'[' bass_figure {
		$$ = $2;
		unsmob_music ($$)->set_mus_property ("bracket-start", SCM_BOOL_T);
	}
	| bass_figure	{
		$$ = $1;
	}
	| br_bass_figure ']' {
		$$ = $1;
		unsmob_music ($1)->set_mus_property ("bracket-stop", SCM_BOOL_T);
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
		Music * m = MY_MAKE_MUSIC("EventChord");
		$2 = scm_reverse_x ($2, SCM_EOL);
		m->set_mus_property ("elements",  $2);
		$$ = m->self_scm ();
	}
	;


optional_rest:
	/**/   { $$ = 0; }
	| REST { $$ = 1; }
	;

simple_element:
	pitch exclamations questions optional_notemode_duration optional_rest {

		Input i = THIS->pop_spot ();
		if (!THIS->lexer_->note_state_b ())
			THIS->parser_error (_ ("Have to be in Note mode for notes"));

		Music *n = 0;
		if ($5)
			n =  MY_MAKE_MUSIC("RestEvent");
		else
			n =  MY_MAKE_MUSIC("NoteEvent");
		
		n->set_mus_property ("pitch", $1);
		n->set_mus_property ("duration", $4);


		if ($3 % 2)
			n->set_mus_property ("cautionary", SCM_BOOL_T);
		if ($2 % 2 || $3 % 2)
			n->set_mus_property ("force-accidental", SCM_BOOL_T);

		Music *v = MY_MAKE_MUSIC("EventChord");
		v->set_mus_property ("elements", scm_list_n (n->self_scm (), SCM_UNDEFINED));
		scm_gc_unprotect_object (n->self_scm());

		v->set_spot (i);
		n->set_spot (i);
		$$ = v;
	}
	| figure_spec optional_notemode_duration {
		Music * m = unsmob_music ($1);
		Input i = THIS->pop_spot (); 
		m->set_spot (i);
		for (SCM s = m->get_mus_property ("elements"); gh_pair_p (s); s = ly_cdr (s))
		{
			unsmob_music (ly_car (s))->set_mus_property ("duration", $2);
		}
		$$ = m;
	}	
 	| RESTNAME optional_notemode_duration		{

		Input i = THIS->pop_spot ();
 		SCM e = SCM_UNDEFINED;
		Music * ev = 0;
 		if (ly_scm2string ($1) =="s") {
			/* Space */
			ev = MY_MAKE_MUSIC("SkipEvent");
		  }
		else {
			ev = MY_MAKE_MUSIC("RestEvent");
		
		    }
		ev->set_mus_property ("duration" ,$2);
		ev->set_spot (i);
 		Music * velt = MY_MAKE_MUSIC("EventChord");
		velt->set_mus_property ("elements", scm_list_n (ev->self_scm (),SCM_UNDEFINED));
		velt->set_spot (i);

 		$$ = velt;
	}
	| MULTI_MEASURE_REST optional_notemode_duration  	{
		THIS->pop_spot ();

		static SCM proc ;
		if (!proc)
			proc = scm_c_eval_string ("make-multi-measure-rest");

		SCM mus = scm_call_2 (proc, $2,
			make_input (THIS->here_input()));	
		scm_gc_protect_object (mus);
		$$ = unsmob_music (mus);
	}
	| STRING optional_notemode_duration 	{
		Input i = THIS->pop_spot ();

		Music * lreq = MY_MAKE_MUSIC("LyricEvent");
                lreq->set_mus_property ("text", $1);
		lreq->set_mus_property ("duration",$2);
		lreq->set_spot (i);
		Music * velt = MY_MAKE_MUSIC("EventChord");
		velt->set_mus_property ("elements", scm_list_n (lreq->self_scm (), SCM_UNDEFINED));

		$$= velt;
	}
	| chord {
		THIS->pop_spot ();

		if (!THIS->lexer_->chord_state_b ())
			THIS->parser_error (_ ("Have to be in Chord mode for chords"));
		$$ = $1;
	}
	;


chord:
	steno_tonic_pitch optional_notemode_duration chord_additions chord_subtractions chord_inversion chord_bass {
                $$ = Chord::get_chord ($1, $3, $4, $5, $6, $2);
		$$->set_spot (THIS->here_input ());
        };

chord_additions: 
	{
		$$ = SCM_EOL;
	} 
	| CHORD_COLON chord_notes {
		$$ = $2;
	}
	;

chord_notes:
	chord_step {
		$$ = $1;
	}
	| chord_notes '.' chord_step {
		$$ = gh_append2 ($$, $3);
	}
	;

chord_subtractions: 
	{
		$$ = SCM_EOL;
	} 
	| CHORD_CARET chord_notes {
		$$ = $2;
	}
	;


chord_inversion:
	{
		$$ = SCM_EOL;
	}
	| CHORD_SLASH steno_tonic_pitch {
		$$ = $2;
	}
	;

chord_bass:
	{
		$$ = SCM_EOL;
	}
	| CHORD_BASS steno_tonic_pitch {
		$$ = $2;
	}
	;

chord_step:
	chord_note {
		$$ = scm_cons ($1, SCM_EOL);
	}
	| CHORDMODIFIER_PITCH {
		$$ = scm_cons (unsmob_pitch ($1)->smobbed_copy (), SCM_EOL);
	}
	| CHORDMODIFIER_PITCH chord_note { /* Ugh. */
		$$ = scm_list_n (unsmob_pitch ($1)->smobbed_copy (),
			$2, SCM_UNDEFINED);
	}
	;

chord_note:
	bare_unsigned {
		 Pitch m;
		m.notename_ = ($1 - 1) % 7;
		m.octave_ = $1 > 7 ? 1 : 0;
		m.alteration_ = 0;

		$$ = m.smobbed_copy ();
        } 
	| bare_unsigned '+' {
		Pitch m;
		m.notename_ = ($1 - 1) % 7;
		m.octave_ = $1 > 7 ? 1 : 0;
		m.alteration_ = 1;


		$$ = m.smobbed_copy ();
	}
	| bare_unsigned CHORD_MINUS {
		Pitch m;
		m.notename_ = ($1 - 1) % 7;
		m.octave_ = $1 > 7 ? 1 : 0;
		m.alteration_ = -1;

		$$ = m.smobbed_copy ();
	}
        ;

/*
	UTILITIES
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
		$$ = gh_int2scm ($1);
	}
	| REAL		{
		$$ = $1;
	}
	| NUMBER_IDENTIFIER		{
		$$ = $1;
	}
	| REAL CM_T	{
		$$ = gh_double2scm (gh_scm2double ($1) CM );
	}
	| REAL PT_T	{
		$$ = gh_double2scm (gh_scm2double ($1) PT);
	}
	| REAL IN_T	{
		$$ = gh_double2scm (gh_scm2double ($1) INCH);
	}
	| REAL MM_T	{
		$$ = gh_double2scm (gh_scm2double ($1) MM);
	}
	| REAL CHAR_T	{
		$$ = gh_double2scm (gh_scm2double ($1) CHAR);
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
			int k = gh_scm2int ($1);
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


string:
	STRING		{
		$$ = $1;
	}
	| STRING_IDENTIFIER	{
		$$ = $1;
	}
	| string '+' string {
		$$ = scm_string_append (scm_list_n ($1, $3, SCM_UNDEFINED));
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


%%

void
My_lily_parser::set_yydebug (bool )
{
#if 0
	yydebug = 1;
#endif
}

extern My_lily_parser * current_parser;

void
My_lily_parser::do_yyparse ()
{
	current_parser = this;;
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
My_lily_parser::beam_check (SCM dur)
{
  Duration *d = unsmob_duration (dur);
  if (unsmob_music (last_beam_start_) && d->duration_log () <= 2)
    {
      Music * m = unsmob_music (last_beam_start_);
      m->origin ()->warning (_("Suspect duration found following this beam"));
    }
  last_beam_start_ = SCM_EOL;
}


/*
It is a little strange, to have this function in this file, but
otherwise, we have to import music classes into the lexer.

*/
int
My_lily_lexer::try_special_identifiers (SCM * destination, SCM sid)
{
	if (gh_string_p (sid)) {
		*destination = sid;
		return STRING_IDENTIFIER;
	} else if (gh_number_p (sid)) {
		*destination = sid;
		return NUMBER_IDENTIFIER;
	} else if (unsmob_translator_def (sid)) {
		*destination = unsmob_translator_def (sid)->clone_scm();
		return TRANSLATOR_IDENTIFIER;
	} else if (unsmob_score (sid)) {
		Score *sc =  new Score (*unsmob_score (sid));
		*destination =sc->self_scm ();
		return SCORE_IDENTIFIER;
	} else if (Music * mus =unsmob_music (sid)) {
		*destination = unsmob_music (sid)->clone ()->self_scm();
		unsmob_music (*destination)->
			set_mus_property ("origin", make_input (last_input_));
		return dynamic_cast<Event*> (mus)
			? EVENT_IDENTIFIER : MUSIC_IDENTIFIER;
	} else if (unsmob_duration (sid)) {
		*destination = unsmob_duration (sid)->smobbed_copy();
		return DURATION_IDENTIFIER;
	} else if (unsmob_music_output_def (sid)) {
		Music_output_def *p = unsmob_music_output_def (sid);
		p = p->clone ();

		*destination = p->self_scm();
		return MUSIC_OUTPUT_DEF_IDENTIFIER;
	}
	return -1;	
}
