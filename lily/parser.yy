%{ // -*-Fundamental-*-

/*
  parser.yy -- Bison/C++ parser for lilypond

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <iostream.h>
#include "translator-def.hh"
#include "lily-guile.hh"
#include "change-iterator.hh"
#include "misc.hh"
#include "my-lily-lexer.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "main.hh"
#include "file-path.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "my-lily-parser.hh"
#include "context-specced-music.hh"
#include "score.hh"
#include "music-list.hh"
#include "output-property-music-iterator.hh"
#include "property-iterator.hh"
#include "file-results.hh"
#include "input.hh"
#include "scope.hh"
#include "relative-music.hh"
#include "lyric-combine-music.hh"
#include "transposed-music.hh"
#include "time-scaled-music.hh"
#include "repeated-music.hh"
#include "lilypond-input-version.hh"
#include "grace-music.hh"
#include "part-combine-music.hh"
#include "scm-hash.hh"
#include "auto-change-iterator.hh"
#include "un-relativable-music.hh"
#include "chord.hh"

bool
is_duration_b (int t)
{
  return t && t == 1 << intlog2 (t);
}


void
set_music_properties (Music *p, SCM a)
{
  for (SCM k = a; gh_pair_p (k); k = gh_cdr (k))
	{
	p->set_mus_property (gh_caar (k), gh_cdar (k));
	}
}


// mmm JUNKME ?
Lilypond_version oldest_version ("1.3.59");

void
print_lilypond_versions (ostream &os)
{
  os << _f ("Oldest supported input version: %s", oldest_version.str ()) 
    << endl;
}


// needed for bison.simple's malloc () and free ()
#include <malloc.h>

#ifndef NDEBUG
#define YYDEBUG 1
#endif

#define YYERROR_VERBOSE 1

#define YYPARSE_PARAM my_lily_parser_l
#define YYLEX_PARAM my_lily_parser_l
#define THIS\
	((My_lily_parser *) my_lily_parser_l)

#define yyerror THIS->parser_error
#define ARRAY_SIZE(a,s)   if (a.size () != s) THIS->parser_error (_f ("Expecting %d arguments", s))

%}


%union {

    Link_array<Request> *reqvec;
    String * string;
    Music *music;
    Score *score;
    Scope *scope;
    Scheme_hash_table *scmhash;
    Musical_req* musreq;
    Music_output_def * outputdef;

    Midi_def* midi;
    Real real;
    Request * request;

    /* We use SCMs to do strings, because it saves us the trouble of
deleting them.  Let's hope that a stack overflow doesnt trigger a move
of the parse stack onto the heap. */

    SCM scm;

    Tempo_req *tempo;
    int i;
}
%{

int
yylex (YYSTYPE *s,  void * v_l)
{
	My_lily_parser	 *pars_l = (My_lily_parser*) v_l;
	My_lily_lexer * lex_l = pars_l->lexer_p_;

	lex_l->lexval_l = (void*) s;
	return lex_l->yylex ();
}


%}

%pure_parser

/* tokens which are not keywords */
%token AUTOCHANGE
%token ALIAS
%token APPLY
%token ARPEGGIO
%token DYNAMICSCRIPT
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
%token ELEMENTDESCRIPTIONS
%token SIMULTANEOUS
%token CONSISTSEND
%token DENIES
%token DURATION
%token EXTENDER
%token FONT
%token GLISSANDO
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
%token NAME
%token PITCHNAMES
%token NOTES
%token PAPER
%token PARTIAL
%token PENALTY
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
%token SCRIPT
%token SKIP
%token SPANREQUEST
%token STYLESHEET
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

/* escaped */
%token E_CHAR E_EXCLAMATION E_SMALLER E_BIGGER E_OPEN E_CLOSE
%token CHORD_BASS CHORD_COLON CHORD_MINUS CHORD_CARET 

%type <i>	exclamations questions dots
%token <i>	DIGIT
%token <scm>	NOTENAME_PITCH
%token <scm>	TONICNAME_PITCH
%token <scm>	CHORDMODIFIER_PITCH
%token <scm>	DURATION_IDENTIFIER
%token <id>	IDENTIFIER


%token <scm>	SCORE_IDENTIFIER
%token <scm>	MUSIC_OUTPUT_DEF_IDENTIFIER

%token <scm>	NUMBER_IDENTIFIER
%token <scm>	REQUEST_IDENTIFIER
%token <scm>	MUSIC_IDENTIFIER TRANSLATOR_IDENTIFIER
%token <scm>	STRING_IDENTIFIER SCM_IDENTIFIER 
%token <scm>	RESTNAME
%token <scm>	STRING 
%token <scm>	SCM_T
%token <i>	UNSIGNED
%token <real>   REAL

%type <outputdef> output_def
%type <scmhash> 	lilypond_header lilypond_header_body
%type <request>	open_request_parens close_request_parens open_request close_request
%type <request> request_with_dir request_that_take_dir verbose_request
%type <i>	sub_quotes sup_quotes
%type <music>	simple_element  request_chord command_element Simple_music  Composite_music 
%type <music>	Alternative_music Repeated_music
%type <i>	tremolo_type
%type <i>	bare_int  bare_unsigned
%type <i>	script_dir

%type <scm>	identifier_init 

%type <scm> steno_duration optional_notemode_duration
%type <scm> entered_notemode_duration explicit_duration
	
%type <reqvec>  pre_requests post_requests
%type <request> gen_text_def
%type <scm>   steno_pitch pitch absolute_pitch
%type <scm>   explicit_pitch steno_tonic_pitch

%type <scm>	chord_additions chord_subtractions chord_notes chord_step
%type <music>	chord
%type <scm>	chord_note chord_inversion chord_bass
%type <scm>	duration_length

%type <scm>  embedded_scm scalar
%type <music>	Music Sequential_music Simultaneous_music Music_sequence
%type <music>	relative_music re_rhythmed_music part_combined_music
%type <music>	property_def translator_change
%type <scm> Music_list
%type <outputdef>  music_output_def_body
%type <request> shorthand_command_req
%type <request>	post_request 
%type <music> command_req verbose_command_req
%type <request>	extender_req
%type <request> hyphen_req
%type <scm>	string bare_number number_expression
%type <score>	score_block score_body

%type <scm>	translator_spec_block translator_spec_body
%type <tempo> 	tempo_request
%type <scm> notenames_body notenames_block chordmodifiers_block
%type <scm>	script_abbreviation



%left '-' '+'
%left '*' '/'
%left UNARY_MINUS

%%

lilypond:	/* empty */
	| lilypond toplevel_expression {}
	| lilypond assignment  { }
	| lilypond error {
		THIS->error_level_i_  = 1;
		//THIS->parser_error (_ ("ly invalid"));
	}
	| lilypond INVALID	{
		THIS->error_level_i_  = 1;
		//THIS->parser_error (_ ("ly invalid"));
	}
	;

toplevel_expression:
	notenames_block			{
		THIS->lexer_p_->pitchname_tab_ =  $1;
	}
	| chordmodifiers_block			{
		THIS->lexer_p_->chordmodifier_tab_  = $1;
	}
	| lilypond_header {
		if (global_header_p)
			scm_unprotect_object (global_header_p->self_scm ());
		global_header_p = $1;
	}
	| score_block {
		score_global_array.push ($1);
		
	}
	| output_def {
		if (dynamic_cast<Paper_def*> ($1))
			THIS->lexer_p_->set_identifier ("$defaultpaper", $1->self_scm ());
		else if (dynamic_cast<Midi_def*> ($1))
			THIS->lexer_p_->set_identifier ("$defaultmidi", $1->self_scm ());
	}
	| embedded_scm {
		// junk value
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
	  for (SCM s = $1; gh_pair_p (s); s = gh_cdr (s)) {
		SCM pt = gh_cdar (s);
		if (!unsmob_pitch (pt))
			THIS->parser_error ("Need pitch object.");
		else
			scm_hashq_set_x (tab, gh_caar (s), pt);
	  }

	  $$ = tab;
	}
	;

lilypond_header_body:
	{
		$$ = new Scheme_hash_table;
		
		Scope *sc = new Scope ($$);
		THIS->lexer_p_-> scope_l_arr_.push (sc);
	}
	| lilypond_header_body assignment semicolon { 

	}
	;

lilypond_header:
	HEADER '{' lilypond_header_body '}'	{
		$$ = $3;
		delete THIS->lexer_p_-> scope_l_arr_.pop ();
	}
	;


/*
	DECLARATIONS
*/
assignment:
	STRING {
		THIS->remember_spot ();
	}
	/* cont */ '=' identifier_init  {
	        THIS->lexer_p_->set_identifier (ly_scm2string ($1), $4);

/*
 TODO: devise standard for protection in parser.

  The parser stack lives on the C-stack, which means that
all objects can be unprotected as soon as they're here.

*/
	/*
		Should find generic way of associating input with objects.
	*/
		THIS->pop_spot ();
	}
	;



identifier_init:
	score_block {
		$$ = $1->self_scm ();
		scm_unprotect_object ($$);
	}
	| output_def {
		$$ = $1->self_scm ();
		scm_unprotect_object ($$);
	}
	| translator_spec_block {
		$$ = $1;
	}
	| Music  {
		$$ = $1->self_scm ();
		scm_unprotect_object ($$);
	}
	| post_request {
		$$ = $1->self_scm ();
		scm_unprotect_object ($$);
	}
	| explicit_duration {
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
		$$ = unsmob_translator_def ($1)->clone_scm ();
		unsmob_translator_def ($$)-> set_spot (THIS->here_input ());
	}
	| TYPE STRING semicolon	{
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
	| translator_spec_body STRING REVERT embedded_scm  {
	  unsmob_translator_def ($$)->add_pop_property (
		scm_string_to_symbol ($2), $4);
	}
	| translator_spec_body STRING '=' identifier_init semicolon	{ 
		SCM v = gh_int2scm (0);
		if (gh_string_p ($4) || gh_number_p ($4) || gh_boolean_p ($4))
			v = $4;
		else 
			THIS->parser_error (_ ("Wrong type for property value"));

		/* ugh*/
		unsmob_translator_def ($$)->add_property_assign ($2, v);
	}
	| translator_spec_body NAME STRING semicolon {
		unsmob_translator_def ($$)->type_name_ = $3;
	}
	| translator_spec_body CONSISTS STRING semicolon {
		unsmob_translator_def ($$)->add_element ($3);
	}
	| translator_spec_body ALIAS STRING semicolon {
		Translator_def*td = unsmob_translator_def ($$);
		td->type_aliases_ = gh_cons ($3, td->type_aliases_);
	}
	| translator_spec_body ELEMENTDESCRIPTIONS embedded_scm {
		for (SCM p = $3; gh_pair_p (p); p = gh_cdr (p))
			unsmob_translator_def ($$)
			->add_property_assign (scm_symbol_to_string (gh_caar (p)), gh_cdar (p));

	}
	| translator_spec_body CONSISTSEND STRING semicolon {
		unsmob_translator_def ($$)->add_last_element ( $3);
	}
	| translator_spec_body ACCEPTS STRING semicolon {
		unsmob_translator_def ($$)->set_acceptor ($3,true);
	}
	| translator_spec_body DENIES STRING semicolon {
		unsmob_translator_def ($$)->set_acceptor ($3,false);
	}
	| translator_spec_body REMOVE STRING semicolon {
		unsmob_translator_def ($$)->remove_element ($3);
	}
	;

/*
	SCORE
*/
score_block:
	SCORE { 
		THIS->remember_spot ();
	}
	/*cont*/ '{' score_body '}' 	{
		THIS->pop_spot ();
		$$ = $4;
		if (!$$->def_p_arr_.size ())
		{
		  Music_output_def *id =
			unsmob_music_output_def (THIS->lexer_p_->lookup_identifier ("$defaultpaper"));
		  $$->add_output (id ? id->clone () :  new Paper_def );
		}
	}
	;

score_body:
	Music	{
		$$ = new Score;
	
		$$->set_spot (THIS->here_input ());
		SCM m = $1->self_scm ();
		scm_unprotect_object (m);
		$$->music_ = m;
	}
	| SCORE_IDENTIFIER {
		$$ = new Score (*unsmob_score ($1));
		$$->set_spot (THIS->here_input ());
	}
	| score_body lilypond_header 	{
		scm_unprotect_object ($2->self_scm ()); 
		$$->header_p_ = $2;
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
		THIS-> lexer_p_-> scope_l_arr_.pop ();
	}
	;

music_output_def_body:
	MIDI '{'    {
	   Music_output_def *id = unsmob_music_output_def (THIS->lexer_p_->lookup_identifier ("$defaultmidi"));

		
	 Midi_def* p =0;
	if (id)
		p = dynamic_cast<Midi_def*> (id->clone ());
	else
		p = new Midi_def;

	 $$ = p;
	 THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
	}
	| PAPER '{' 	{
		Music_output_def *id = unsmob_music_output_def (THIS->lexer_p_->lookup_identifier ("$defaultpaper"));
		  Paper_def *p = 0;
		if (id)
			p = dynamic_cast<Paper_def*> (id->clone ());
		else
			p = new Paper_def;
		THIS-> lexer_p_-> scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| PAPER '{' MUSIC_OUTPUT_DEF_IDENTIFIER 	{
		Music_output_def *p = unsmob_music_output_def ($3);
		p = p->clone ();
		THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| MIDI '{' MUSIC_OUTPUT_DEF_IDENTIFIER 	{
		Music_output_def *p = unsmob_music_output_def ($3);
		p = p->clone ();

		THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| music_output_def_body assignment semicolon {

	}
	| music_output_def_body translator_spec_block	{
		$$->assign_translator ($2);
	}
	| music_output_def_body STYLESHEET embedded_scm {
		dynamic_cast<Paper_def*> ($$)-> style_sheet_ = $3;
	}
	| music_output_def_body tempo_request semicolon {
		/*
			junk this ? there already is tempo stuff in
			music.
		*/
		int m = gh_scm2int ( $2->get_mus_property ("metronome-count"));
		Duration *d = unsmob_duration ($2->get_mus_property ("duration"));
		dynamic_cast<Midi_def*> ($$)->set_tempo (d->length_mom (), m);
	}
	| music_output_def_body error {

	}
	;

tempo_request:
	TEMPO steno_duration '=' bare_unsigned	{
		$$ = new Tempo_req;
		$$->set_mus_property ("duration", $2);
		$$->set_mus_property ("metronome-count", gh_int2scm ( $4));
	}
	;

Music_list: /* empty */ {
		$$ = gh_cons (SCM_EOL, SCM_EOL);
	}
	| Music_list Music {
		SCM s = $$;
		SCM c = gh_cons ($2->self_scm (), SCM_EOL);
		scm_unprotect_object ($2->self_scm ()); /* UGH */
		if (gh_pair_p (gh_cdr (s)))
			gh_set_cdr_x (gh_cdr (s), c); /* append */
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
		$$ = 0;
	}
	| ALTERNATIVE Music_sequence {
		$$ = $2;
		$2->set_spot (THIS->here_input ());
	}
	;




Repeated_music:
	REPEAT string bare_unsigned Music Alternative_music
	{
		Music_sequence* alts = dynamic_cast <Music_sequence*> ($5);
		if (alts && $3 < alts->length_i ())
			$5->origin ()->warning (_ ("More alternatives than repeats.  Junking excess alternatives."));
		Music *beg = $4;
		int times = $3;

		Repeated_music * r = new Repeated_music (SCM_EOL);

		if (beg)
			{
			r-> set_mus_property ("body", beg->self_scm ());
			scm_unprotect_object (beg->self_scm ());
			}
		r->set_mus_property ("repeat-count", gh_int2scm (times >? 1));

		if (alts)
			{
			alts->truncate (times);
			r-> set_mus_property ("alternatives", alts->self_scm ());
			scm_unprotect_object (alts->self_scm ());  
			}
		SCM func = scm_eval2 (ly_symbol2scm ("repeat-name-to-ctor"), SCM_EOL);
		SCM result = gh_call1 (func, $2);

		set_music_properties (r, result);

		r->set_spot (*$4->origin ());
		$$ = r;
	}
	;

Music_sequence: '{' Music_list '}'	{
		$$ = new Music_sequence (SCM_EOL);
		$$->set_mus_property ("elements", gh_car ($2));
	}
	;

Sequential_music:
	SEQUENTIAL '{' Music_list '}'		{
		$$ = new Sequential_music (SCM_EOL);
		$$->set_mus_property ("elements", gh_car ($3));
	}
	| '{' Music_list '}'		{
		$$ = new Sequential_music (SCM_EOL);
		$$->set_mus_property ("elements", gh_car ($2));
	}
	;

Simultaneous_music:
	SIMULTANEOUS '{' Music_list '}'{
		$$ = new Simultaneous_music (SCM_EOL);
		$$->set_mus_property ("elements", gh_car ($3));

	}
	| '<' Music_list '>'	{
		$$ = new Simultaneous_music (SCM_EOL);
		$$->set_mus_property ("elements", gh_car ($2));
	}
	;

Simple_music:
	request_chord		{ $$ = $1; }
	| OUTPUTPROPERTY embedded_scm embedded_scm '=' embedded_scm	{
		SCM pred = $2;
		if (!gh_symbol_p ($3))
		{
			THIS->parser_error (_ ("Second argument must be a symbol")); 
		}
		/*hould check # args */
		if (!gh_procedure_p (pred))
		{
			THIS->parser_error (_ ("First argument must be a procedure taking 1 argument"));
		}

		Music *m = new Music (SCM_EOL);
		m->set_mus_property ("predicate", pred);
		m->set_mus_property ("symbol", $3);
		m->set_mus_property ("value",  $5);
		m->set_mus_property ("iterator-ctor",
		Output_property_music_iterator::constructor_cxx_function);

		$$ = m;
	}
	| MUSIC_IDENTIFIER {
		$$ = unsmob_music ($1)->clone ();
	}
	| property_def
	| translator_change
	| Simple_music '*' bare_unsigned '/' bare_unsigned 	{
		$$ = $1;
		$$->compress (Moment ($3, $5 ));
	}
	| Simple_music '*' bare_unsigned		 {
		$$ = $1;
		$$->compress (Moment ($3, 1));
	}
	;


Composite_music:
	CONTEXT STRING Music	{
		Context_specced_music *csm =  new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", $3->self_scm ());
		scm_unprotect_object ($3->self_scm ());

		csm->set_mus_property ("context-type",$2);
		csm->set_mus_property ("context-id", ly_str02scm (""));

		$$ = csm;
	}
	| AUTOCHANGE STRING Music	{
		Music * chm = new Music_wrapper (SCM_EOL);
		chm->set_mus_property ("element", $3->self_scm ());
		chm->set_mus_property ("iterator-ctor", Auto_change_iterator::constructor_cxx_function);

		scm_unprotect_object ($3->self_scm ());
		chm->set_mus_property ("what", $2); 

		$$ = chm;
		chm->set_spot (*$3->origin ());
	}
	| GRACE Music {
		$$ = new Grace_music (SCM_EOL);
		$$->set_mus_property ("element", $2->self_scm ());
		scm_unprotect_object ($2->self_scm ());

	}
	| CONTEXT STRING '=' STRING Music {
		Context_specced_music *csm =  new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", $5->self_scm ());
		scm_unprotect_object ($5->self_scm ());

		csm->set_mus_property ("context-type", $2);
		csm->set_mus_property ("context-id", $4);

		$$ = csm;
	}
	| TIMES {
		THIS->remember_spot ();
	}
	/* CONTINUED */ 
		bare_unsigned '/' bare_unsigned Music 	

	{
		int n = $3; int d = $5;
		Music *mp = $6;
		$$ = new Time_scaled_music (SCM_EOL);
		$$->set_spot (THIS->pop_spot ());


		$$->set_mus_property ("element", mp->self_scm ());
		scm_unprotect_object (mp->self_scm ());
		$$->set_mus_property ("numerator", gh_int2scm (n));
		$$->set_mus_property ("denominator", gh_int2scm (d));
		$$->compress (Moment (n,d));

	}
	| Repeated_music		{ $$ = $1; }
	| Simultaneous_music		{ $$ = $1; }
	| Sequential_music		{ $$ = $1; }
	| TRANSPOSE pitch Music {
		$$ = new Transposed_music (SCM_EOL);
		Music *p = $3;
		Pitch pit = *unsmob_pitch ($2);

		p->transpose (pit);
		$$->set_mus_property ("element", p->self_scm ());
		scm_unprotect_object (p->self_scm ());
	}
	| TRANSPOSE steno_tonic_pitch Music {
		$$ = new Transposed_music (SCM_EOL);
		Music *p = $3;
		Pitch pit = *unsmob_pitch ($2);

		p->transpose (pit);
		$$->set_mus_property ("element", p->self_scm ());
		scm_unprotect_object (p->self_scm ());
	
	}
	| APPLY embedded_scm Music  {
		SCM ret = gh_call1 ($2, $3->self_scm ());
		Music *m = unsmob_music (ret);
		if (!m) {
			THIS->parser_error ("\\apply must return a Music");
			m = new Music (SCM_EOL);
			}
		$$ = m;
	}
	| NOTES
		{ THIS->lexer_p_->push_note_state (); }
	Music
		{ $$ = $3;
		  THIS->lexer_p_->pop_state ();
		}
	| CHORDS
		{ THIS->lexer_p_->push_chord_state (); }
	Music
		{
		  Music * chm = new Un_relativable_music ;
		  chm->set_mus_property ("element", $3->self_scm ());
		  $$ = chm;

		  THIS->lexer_p_->pop_state ();
	}
	| LYRICS
		{ THIS->lexer_p_->push_lyric_state (); }
	Music
		{
		  $$ = $3;
		  THIS->lexer_p_->pop_state ();
	}
	| relative_music	{ $$ = $1; }
	| re_rhythmed_music	{ $$ = $1; } 
	| part_combined_music	{ $$ = $1; } 
	;

relative_music:
	RELATIVE absolute_pitch Music {
		Music * p = $3;
		Pitch pit = *unsmob_pitch ($2);
		$$ = new Relative_octave_music (SCM_EOL);

		$$->set_mus_property ("element", p->self_scm ());
		scm_unprotect_object (p->self_scm ());

		$$->set_mus_property ("last-pitch", p->to_relative_octave (pit).smobbed_copy ());

	}
	;

re_rhythmed_music:
	ADDLYRICS Music Music {
	  Lyric_combine_music * l = new Lyric_combine_music (SCM_EOL);
	  l->set_mus_property ("music", $2->self_scm ());
	  l->set_mus_property ("lyrics", $3->self_scm ());
	  scm_unprotect_object ($3->self_scm ());
	  scm_unprotect_object ($2->self_scm ());
	  $$ = l;
	}
	;

part_combined_music:
	PARTCOMBINE STRING Music Music {
		Part_combine_music * p = new Part_combine_music (SCM_EOL);

		p->set_mus_property ("what", $2);
		p->set_mus_property ("one", $3->self_scm ());
		p->set_mus_property ("two", $4->self_scm ());  

		scm_unprotect_object ($3->self_scm ());
		scm_unprotect_object ($4->self_scm ());  


		$$ = p;
	}
	;

translator_change:
	TRANSLATOR STRING '=' STRING  {
		Music * t = new Music (SCM_EOL);
		t->set_mus_property ("iterator-ctor",
			Change_iterator::constructor_cxx_function);
		t-> set_mus_property ("change-to-type", $2);
		t-> set_mus_property ("change-to-id", $4);

		$$ = t;
		$$->set_spot (THIS->here_input ());
	}
	;

property_def:
	PROPERTY STRING '.' STRING '='  scalar {
		Music *t = new Music (SCM_EOL);

		t->set_mus_property ("iterator-ctor",
			Property_iterator::constructor_cxx_function);
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));
		t->set_mus_property ("value", $6);

		Context_specced_music *csm = new Context_specced_music (SCM_EOL);

		csm->set_mus_property ("element", t->self_scm ());
		scm_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING UNSET {
		Music *t = new Music (SCM_EOL);

		t->set_mus_property ("iterator-ctor",
			Property_unset_iterator::constructor_cxx_function);
		t->set_mus_property ("symbol", scm_string_to_symbol ($4));

		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING SET embedded_scm '=' embedded_scm {
		Music *t = new Music (SCM_EOL);
		t->set_mus_property ("iterator-ctor",
			Push_property_iterator::constructor_cxx_function);
		t->set_mus_property ("symbols", scm_string_to_symbol ($4));
		t->set_mus_property ("pop-first", SCM_BOOL_T);
		t->set_mus_property ("grob-property", $6);
		t->set_mus_property ("grob-value", $8);
		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_unprotect_object (t->self_scm ());
		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING OVERRIDE embedded_scm '=' embedded_scm {
		Music *t = new Music (SCM_EOL);
		t->set_mus_property ("iterator-ctor",
			Push_property_iterator::constructor_cxx_function);
		t->set_mus_property ("symbols", scm_string_to_symbol ($4));
		t->set_mus_property ("grob-property", $6);
		t->set_mus_property ("grob-value", $8);
		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	| PROPERTY STRING '.' STRING REVERT embedded_scm {
		Music *t = new Music (SCM_EOL);
		t->set_mus_property ("iterator-ctor",
			Pop_property_iterator::constructor_cxx_function);
		t->set_mus_property ("symbols", scm_string_to_symbol ($4));
		t->set_mus_property ("grob-property", $6);

		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm-> set_mus_property ("context-type", $2);
	}
	;

scalar:
	string		{ $$ = $1; }
	| bare_int	{ $$ = gh_int2scm ($1); }
	| embedded_scm 	{ $$ = $1; }
	;


request_chord:
	pre_requests simple_element post_requests	{
		Music_sequence *l = dynamic_cast<Music_sequence*> ($2);
		if (l) {
			for (int i=0; i < $1->size (); i++)
				l->append_music ($1->elem (i));
			for (int i=0; i < $3->size (); i++)
				l->append_music ($3->elem (i));
			}
		else
			programming_error ("Need Sequence to add music to");
 		$$ = $2;
		
	}
	| command_element
	;

command_element:
	command_req {
		$$ = new Request_chord (SCM_EOL);
		$$->set_mus_property ("elements", gh_cons ($1->self_scm (), SCM_EOL));
		$$-> set_spot (THIS->here_input ());
		$1-> set_spot (THIS->here_input ());
	}
	| BAR STRING ';' 			{
		Music *t = new Music (SCM_EOL);

		t->set_mus_property ("iterator-ctor",
			Property_iterator::constructor_cxx_function);
		t->set_mus_property ("symbol", ly_symbol2scm ("whichBar"));
		t->set_mus_property ("value", $2);

		Context_specced_music *csm = new Context_specced_music (SCM_EOL);
		csm->set_mus_property ("element", t->self_scm ());
		scm_unprotect_object (t->self_scm ());

		$$ = csm;
		$$->set_spot (THIS->here_input ());

		csm->set_mus_property ("context-type", ly_str02scm ("Score"));
	}
	| PARTIAL duration_length ';' 	{
		Music * p = new Music (SCM_EOL);
		p->set_mus_property ("symbol", ly_symbol2scm ( "measurePosition"));
		p->set_mus_property ("iterator-ctor",
			Property_iterator::constructor_cxx_function);

		Moment m = - unsmob_duration ($2)->length_mom ();
		p->set_mus_property ("value", m.smobbed_copy ());

		Context_specced_music * sp = new Context_specced_music (SCM_EOL);
		sp->set_mus_property ("element", p->self_scm ());
		scm_unprotect_object (p->self_scm ());

		$$ =sp ;
		sp-> set_mus_property ("context-type", ly_str02scm ( "Score"));
	}
	| CLEF STRING ';' {
		SCM func = scm_eval2 (ly_symbol2scm ("clef-name-to-properties"), SCM_EOL);
		SCM result = gh_call1 (func, $2);

		SCM l = SCM_EOL;
		for (SCM s = result ; gh_pair_p (s); s = gh_cdr (s)) {
			Music * p = new Music (SCM_EOL);
			set_music_properties (p, gh_car (s));
			l = gh_cons (p->self_scm (), l);
			scm_unprotect_object (p->self_scm ());
		}
		Sequential_music * seq = new Sequential_music (SCM_EOL);
		seq->set_mus_property ("elements", l);

		Context_specced_music * sp = new Context_specced_music (SCM_EOL);
		sp->set_mus_property ("element", seq->self_scm ());
		scm_unprotect_object (seq->self_scm ());

		$$ =sp ;
		sp-> set_mus_property ("context-type", ly_str02scm ("Staff"));
	}
	| TIME_T bare_unsigned '/' bare_unsigned ';' {
		Music * p = new Music (SCM_EOL);
		p->set_mus_property ("symbol",
			ly_symbol2scm ( "timeSignatureFraction"));
		p->set_mus_property ("iterator-ctor",
			Property_iterator::constructor_cxx_function);

		p->set_mus_property ("value", gh_cons (gh_int2scm ($2),
							gh_int2scm ($4)));

		Context_specced_music * sp = new Context_specced_music (SCM_EOL);
		sp->set_mus_property ("element", p->self_scm ());
		scm_unprotect_object (p->self_scm ());

		$$ = sp;

/*
 TODO: should make alias TimingContext for Score
*/

		sp-> set_mus_property ("context-type", ly_str02scm ( "Score"));
	}
	;

command_req:
	shorthand_command_req  	{ $$ = $1; }
	| verbose_command_req semicolon	{ $$ = $1; }
	;

shorthand_command_req:
	extender_req {
		$$ = $1;
	}
	| hyphen_req {
		$$ = $1;
	}
	| '|'				{
		$$ = new Barcheck_req;
	}
	| '~'	{
		$$ = new Tie_req;
	}
	| '['		{
		Span_req*b= new Span_req;
		b->set_span_dir (START);
		b->set_mus_property ("span-type", ly_str02scm ("beam"));
		$$ =b;
	}
	| ']'		{
		Span_req*b= new Span_req;
		b->set_span_dir ( STOP);
		b->set_mus_property ("span-type", ly_str02scm ("beam"));
		$$ = b;
	}
	| BREATHE {
		$$ = new Breathing_sign_req;
	}
	;


verbose_command_req:
	COMMANDSPANREQUEST bare_int STRING { /*TODO: junkme */
		Span_req * sp_p = new Span_req;
		sp_p-> set_span_dir ( Direction ($2));
		sp_p->set_mus_property ("span-type",$3);
		sp_p->set_spot (THIS->here_input ());
		$$ = sp_p;
	}
	| MARK  {
		Mark_req * m = new Mark_req;
		$$ = m;
	}
	| MARK scalar {
		Mark_req *m = new Mark_req;
		m->set_mus_property ("label", $2);
		$$ = m;

	}
	| PENALTY bare_int 	{
		Break_req * b = new Break_req;
		b->set_mus_property ("penalty", gh_double2scm ( $2 / 100.0));
		b->set_spot (THIS->here_input ());
		$$ = b;
	}
	| SKIP duration_length {
		Skip_req * skip_p = new Skip_req;
		skip_p->set_mus_property ("duration", $2);

		$$ = skip_p;
	}
	| tempo_request {
		$$ = $1;
	}
	| KEY {
		Key_change_req *key_p= new Key_change_req;
		$$ = key_p;
	}
	| KEY NOTENAME_PITCH SCM_IDENTIFIER 	{
		Key_change_req *key_p= new Key_change_req;
		
		key_p->set_mus_property ("pitch-alist", $3);
 ((Music* )key_p)->transpose (* unsmob_pitch ($2));
		$$ = key_p; 
	}
	;

post_requests:
	{
		$$ = new Link_array<Request>;
	}
	| post_requests post_request {
		$2->set_spot (THIS->here_input ());
		$$->push ($2);
	}
	;

post_request:
	verbose_request
	| request_with_dir
	| close_request
	;


request_that_take_dir:
	gen_text_def
	| verbose_request
	| script_abbreviation {
		SCM s = THIS->lexer_p_->lookup_identifier ("dash-" + ly_scm2string ($1));
		Articulation_req *a = new Articulation_req;
		if (gh_string_p (s))
			a->set_mus_property ("articulation-type", s);
		else THIS->parser_error (_ ("Expecting string as script definition"));
		$$ = a;
	}
	;

request_with_dir:
	script_dir request_that_take_dir	{
		if (Script_req * gs = dynamic_cast<Script_req*> ($2))
			gs->set_direction (Direction ($1));
		else if ($1)
			$2->origin ()->warning (_ ("Can't specify direction for this request"));
		$$ = $2;
	}
	;
	
verbose_request:
	REQUEST_IDENTIFIER	{
		$$ = dynamic_cast<Request*> (unsmob_music ($1)->clone ());
		$$->set_spot (THIS->here_input ());
	}
	| DYNAMICSCRIPT embedded_scm {
		/*
			TODO: junkme, use text-type == dynamic
		*/
		Text_script_req *d = new Text_script_req;
		d->set_mus_property ("text-type" , ly_symbol2scm ("dynamic"));
		d->set_mus_property ("text", $2);
		d->set_spot (THIS->here_input ());
		$$ = d;
	}
	| SPANREQUEST bare_int STRING {
		Span_req * sp_p = new Span_req;
		sp_p->set_span_dir ( Direction ($2));
		sp_p->set_mus_property ("span-type", $3);
		sp_p->set_spot (THIS->here_input ());
		$$ = sp_p;
	}
	| tremolo_type  {
               Tremolo_req* a = new Tremolo_req;
               a->set_spot (THIS->here_input ());
               a->set_mus_property ("tremolo-type", gh_int2scm ($1));
               $$ = a;
        }
	| SCRIPT STRING 	{ 
		Articulation_req * a = new Articulation_req;
		a->set_mus_property ("articulation-type", $2);
		a->set_spot (THIS->here_input ());
		$$ = a;
	}
	/*
duh, junk this syntax from the parser, if possible. 
	*/
	| ARPEGGIO {
		Arpeggio_req *a = new Arpeggio_req;
		a->set_spot (THIS->here_input ());
		$$ = a;
	}
	| GLISSANDO {
		Glissando_req *g = new Glissando_req;
		g->set_spot /* No pun intended */ (THIS->here_input ());
		$$ = g;
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
		p.octave_i_ +=  $2;
		$$ = p.smobbed_copy ();
	}
	| NOTENAME_PITCH sub_quotes	 {
		Pitch p =* unsmob_pitch ($1);

		p.octave_i_ +=  -$2;
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
		p.octave_i_ +=  $2;
		$$ = p.smobbed_copy ();
	}
	| TONICNAME_PITCH sub_quotes	 {
		Pitch p =* unsmob_pitch ($1);

		p.octave_i_ +=  -$2;
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

explicit_duration:
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
		if (!THIS->lexer_p_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = new Extender_req;
	}
	;

hyphen_req:
	HYPHEN {
		if (!THIS->lexer_p_->lyric_state_b ())
			THIS->parser_error (_ ("Have to be in Lyric mode for lyrics"));
		$$ = new Hyphen_req;
	}
	;

close_request:
	close_request_parens {
		$$ = $1;
		dynamic_cast<Span_req*> ($$)->set_span_dir ( START);
	}
	
close_request_parens:
	'('	{
		Span_req* s= new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", ly_str02scm ( "slur"));
	}
	| E_OPEN	{
		Span_req* s= new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", ly_str02scm ( "phrasing-slur"));
	}
	| E_SMALLER {
		Span_req*s =new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", ly_str02scm ( "crescendo"));
	}
	| E_BIGGER {
		Span_req*s =new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", ly_str02scm ("decrescendo"));
	}
	;


open_request:
	open_request_parens {
		$$ = $1;
		dynamic_cast<Span_req*> ($$)->set_span_dir (STOP);
	}
	;

open_request_parens:
	E_EXCLAMATION 	{
		Span_req *s =  new Span_req;
		s->set_mus_property ("span-type", ly_str02scm ( "crescendo"));

		$$ = s;
	}
	| ')'	{
		Span_req* s= new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", ly_str02scm ( "slur"));
	}
	| E_CLOSE	{
		Span_req* s= new Span_req;
		$$ = s;
		s->set_mus_property ("span-type", ly_str02scm ( "phrasing-slur"));
	}
	;

gen_text_def:
	embedded_scm {
		Text_script_req *t = new Text_script_req;
		t->set_mus_property ("text", $1);
		t->set_spot (THIS->here_input ());
		$$ = t;
	}
	| string {
		Text_script_req *t = new Text_script_req;
		t->set_mus_property ("text", $1);
		t->set_spot (THIS->here_input ());
		$$ = t;
	}
	| DIGIT {
		String ds = to_str ($1);
		Text_script_req* t = new Text_script_req;

		t->set_mus_property ("text",  ly_str02scm (ds.ch_C ()));
		t->set_mus_property ("text-type" , ly_symbol2scm ("finger"));
		t->set_spot (THIS->here_input ());
		$$ = t;
	}
	;

script_abbreviation:
	'^'		{
		$$ = gh_str02scm ("hat");
	}
	| '+'		{
		$$ = gh_str02scm ("plus");
	}
	| '-' 		{
		$$ = gh_str02scm ("dash");
	}
 	| '|'		{
		$$ = gh_str02scm ("bar");
	}
	| '>'		{
		$$ = gh_str02scm ("larger");
	}
	| '.' 		{
		$$ = gh_str02scm ("dot");
	}
	;


script_dir:
	'_'	{ $$ = DOWN; }
	| '^'	{ $$ = UP; }
	| '-'	{ $$ = CENTER; }
	;

pre_requests:
	{
		$$ = new Link_array<Request>;
	}
	| pre_requests open_request {
		$$->push ($2);
	}
	;

absolute_pitch:
	steno_pitch	{
		$$ = $1;
	}
	;

duration_length:
	steno_duration {
		$$ = $1;
	}
	| explicit_duration {
		$$ = $1;
	}	
	| duration_length '*' bare_unsigned {
		$$ = unsmob_duration ($$)->compressed ( $3) .smobbed_copy ();
	}
	| duration_length '/' bare_unsigned {
		$$ = unsmob_duration ($$)->compressed (Moment (1,$3)).smobbed_copy ();
	}
	;

entered_notemode_duration:
	steno_duration	{
		THIS->set_last_duration (unsmob_duration ($1));
	}
	;

optional_notemode_duration:
	{
		$$ = THIS->default_duration_.smobbed_copy ();
	}
	| entered_notemode_duration {
		$$ = $1;
	}
	| explicit_duration {
		$$ = $1;
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
		$$ = k.smobbed_copy ();		
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


simple_element:
	pitch exclamations questions optional_notemode_duration {
		if (!THIS->lexer_p_->note_state_b ())
			THIS->parser_error (_ ("Have to be in Note mode for notes"));

		Note_req *n = new Note_req;
		
		n->set_mus_property ("pitch", $1);
		n->set_mus_property ("duration", $4);

		if ($3 % 2)
			n->set_mus_property ("cautionary", SCM_BOOL_T);
		if ($2 % 2 || $3 % 2)
			n->set_mus_property ("force-accidental", SCM_BOOL_T);

		Simultaneous_music*v = new Request_chord (SCM_EOL);
		v->set_mus_property ("elements", gh_list (n->self_scm (), SCM_UNDEFINED));
		
/*
FIXME: location is one off, since ptich & duration don't contain origin refs. 
*/
		v->set_spot (THIS->here_input ());
		n->set_spot (THIS->here_input ());

		$$ = v;
	}
	| RESTNAME optional_notemode_duration		{

		SCM e = SCM_UNDEFINED;
		  if (ly_scm2string ($1) =="s")
		    { /* Space */
		      Skip_req * skip_p = new Skip_req;
		      skip_p->set_mus_property ("duration" ,$2);

		      skip_p->set_spot (THIS->here_input ());
			e = skip_p->self_scm ();
		    }
		  else
		    {
		      Rest_req * rest_req_p = new Rest_req;
		      rest_req_p->set_mus_property ("duration", $2);
		      rest_req_p->set_spot (THIS->here_input ());
			e = rest_req_p->self_scm ();
		    }
		  Simultaneous_music* velt_p = new Request_chord (SCM_EOL);
		velt_p-> set_mus_property ("elements", gh_list (e,SCM_UNDEFINED));
		  velt_p->set_spot (THIS->here_input ());


		  $$ = velt_p;
	}
	| MULTI_MEASURE_REST optional_notemode_duration  	{
		Skip_req * sk = new Skip_req;
		sk->set_mus_property ("duration", $2);
		Span_req *sp1 = new Span_req;
		Span_req *sp2 = new Span_req;
		sp1-> set_span_dir ( START);
		sp2-> set_span_dir ( STOP);
		SCM r = ly_str02scm ("rest");
		sp1->set_mus_property ("span-type", r);
		sp2->set_mus_property ("span-type", r);

		Request_chord * rqc1 = new Request_chord (SCM_EOL);
		rqc1->set_mus_property ("elements", gh_list (sp1->self_scm (), SCM_UNDEFINED));
		Request_chord * rqc2 = new Request_chord (SCM_EOL);
		rqc2->set_mus_property ("elements", gh_list (sk->self_scm (), SCM_UNDEFINED));;
		Request_chord * rqc3 = new Request_chord (SCM_EOL);
		rqc3->set_mus_property ("elements", gh_list (sp2->self_scm (), SCM_UNDEFINED));;

		SCM ms = gh_list (rqc1->self_scm (), rqc2->self_scm (), rqc3->self_scm (), SCM_UNDEFINED);

		$$ = new Sequential_music (SCM_EOL);
		$$->set_mus_property ("elements", ms);
	}
	| STRING { 
	 	THIS->remember_spot ();
	} 
	/* cont */
	optional_notemode_duration 	{
		if (!THIS->lexer_p_->lyric_state_b ()) {
			THIS->pop_spot ().error (_ ("Have to be in Lyric mode for lyrics"));
			THIS->error_level_i_  = 1;
			THIS->parser_error (_ ("Giving up"));
		} 
		else
			THIS->pop_spot ();
		Lyric_req* lreq_p = new Lyric_req;
                lreq_p->set_mus_property ("text", $1);
		lreq_p->set_mus_property ("duration",$3);
		lreq_p->set_spot (THIS->here_input ());
		Simultaneous_music* velt_p = new Request_chord (SCM_EOL);
		velt_p->set_mus_property ("elements", gh_list (lreq_p->self_scm (), SCM_UNDEFINED));


		$$= velt_p;

	}
	| chord {
		if (!THIS->lexer_p_->chord_state_b ())
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
		$$ = $1
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
	| '/' steno_tonic_pitch {
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
		$$ = gh_cons ($1, SCM_EOL);
	}
	| CHORDMODIFIER_PITCH {
		$$ = gh_cons ($1, SCM_EOL);
	}
	| CHORDMODIFIER_PITCH chord_note { /* Ugh. */
		$$ = gh_list ($1, $2, SCM_UNDEFINED);
	}
	;

chord_note:
	bare_unsigned {
		 Pitch m;
		m.notename_i_ = ($1 - 1) % 7;
		m.octave_i_ = $1 > 7 ? 1 : 0;
		m.alteration_i_ = 0;

		$$ = m.smobbed_copy ();
        } 
	| bare_unsigned '+' {
		Pitch m;
		m.notename_i_ = ($1 - 1) % 7;
		m.octave_i_ = $1 > 7 ? 1 : 0;
		m.alteration_i_ = 1;


		$$ = m.smobbed_copy ();
	}
	| bare_unsigned CHORD_MINUS {
		Pitch m;
		m.notename_i_ = ($1 - 1) % 7;
		m.octave_i_ = $1 > 7 ? 1 : 0;
		m.alteration_i_ = -1;

		$$ = m.smobbed_copy ();
	}
        ;

/*
	UTILITIES
 */
number_expression:
	bare_number {
		$$ = $1;
	}
	| '-'  number_expression %prec UNARY_MINUS {
		$$ = scm_difference ($2, SCM_UNDEFINED);
	}
	| number_expression '*' number_expression {
		$$ = scm_product ($1, $3);
	}
	| number_expression '/' number_expression {
		$$ = scm_divide ($1, $3);
	}
	| number_expression '+' number_expression {
		$$ = scm_sum ($1, $3);
	}
	| number_expression '-' number_expression {
		$$ = scm_difference ($1, $3);
	}
	| '(' number_expression ')'	{
		$$ = $2;
	}
	;

bare_number:
	UNSIGNED	{
		$$ = gh_int2scm ($1);
	}
	| DIGIT		{
		$$ = gh_int2scm ($1);
	}
	| REAL		{
		$$ = gh_double2scm ($1);
	}
	| NUMBER_IDENTIFIER		{
		$$ = $1;
	}
	| REAL CM_T	{
		$$ = gh_double2scm ($1 CM);
	}
	| REAL PT_T	{
		$$ = gh_double2scm ($1 PT);
	}
	| REAL IN_T	{
		$$ = gh_double2scm ($1 INCH);
	}
	| REAL MM_T	{
		$$ = gh_double2scm ($1 MM);
	}
	| REAL CHAR_T	{
		$$ = gh_double2scm ($1 CHAR);
	}
	;


bare_unsigned:
	bare_number {
		if (scm_integer_p ($1) == SCM_BOOL_T) {
			$$ = gh_scm2int ($1);

		} else {
			THIS->parser_error (_ ("need integer number arg"));
			$$ = 0;
		}
		if ($$ < 0) {
			THIS->parser_error (_ ("Must be positive integer"));
			$$ = -$$;
			}

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
		$$ = scm_string_append (scm_listify ($1, $3, SCM_UNDEFINED));
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


semicolon:
	';'
	;

%%

void
My_lily_parser::set_yydebug (bool b)
{
#ifdef YYDEBUG
	yydebug = b;
#endif
}

extern My_lily_parser * current_parser;

void
My_lily_parser::do_yyparse ()
{

	current_parser = this;;
	yyparse ((void*)this);
}


