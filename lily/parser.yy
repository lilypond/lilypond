%{ // -*-Fundamental-*-

/*
  parser.yy -- Bison/C++ parser for mudela

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <iostream.h>
#include "lily-guile.hh"
#include "notename-table.hh"
#include "scalar.hh"
#include "translation-property.hh"
#include "script-def.hh"
#include "lookup.hh"
#include "misc.hh"
#include "my-lily-lexer.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "main.hh"
#include "file-path.hh"
#include "keyword.hh"
#include "debug.hh"
#include "parseconstruct.hh"
#include "dimensions.hh"
#include "identifier.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "my-lily-parser.hh"
#include "text-def.hh"
#include "translator-group.hh"
#include "score.hh"
#include "music-list.hh"
#include "duration-convert.hh"
#include "change-translator.hh"
#include "file-results.hh"
#include "mudela-version.hh"
#include "scope.hh"
#include "relative-music.hh"
#include "transposed-music.hh"
#include "compressed-music.hh"
#include "repeated-music.hh"

// mmm
Mudela_version oldest_version ("1.0.14");
Mudela_version version ("1.0.15");


// needed for bison.simple's malloc() and free()
#include <malloc.h>

struct Assignment {
	String *name_p_;
	Identifier *id_p_;
	~Assignment () {
		delete name_p_;
		delete id_p_;
	}
	Assignment () {
		name_p_ = 0;
		id_p_ =0;
	}
	Assignment (Assignment const&s)
	{
		name_p_ = new String (*s.name_p_);
		id_p_ = s.id_p_->clone ();
	}
};

Paper_def* current_paper = 0;

#ifndef NDEBUG
#define YYDEBUG 1
#endif

#define YYERROR_VERBOSE 1

#define YYPARSE_PARAM my_lily_parser_l
#define YYLEX_PARAM my_lily_parser_l
#define THIS ((My_lily_parser *) my_lily_parser_l)

#define yyerror THIS->parser_error
#define ARRAY_SIZE(a,s)   if (a.size () != s) THIS->parser_error (_f("expecting %d arguments", s))

%}


%union {
    Array<Interval>* intarr;
    Array<Musical_pitch> *pitch_arr;
    Array<String> * strvec;
    Array<int> *intvec;
    Notename_table *chordmodifiertab;
    Duration *duration;
    General_script_def * script;
    Identifier *id;
    Music *music;
    Music_list *music_list;
    Score *score;
    Scope *scope;
    Interval *interval;
    Musical_req* musreq;
    Music_output_def * outputdef;
    Musical_pitch * pitch;
    Midi_def* midi;
    Moment *moment;
    Note_req *notereq;
    Notename_table *notenametab;
    Paper_def *paper;
    Real real;
    Request * request;
    Scalar *scalar;
    Simultaneous_music *chord;
    String *string;
    Tempo_req *tempo;
    Text_def * textdef;
    Translator* trans;
    char c;
    const char *consstr;
    int i;
    int ii[10];
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

%token ABSDYNAMIC
%token ACCEPTS
%token ALTERNATIVE
%token BAR
%token CADENZA
%token CHORDMODIFIERS
%token CHORDS
%token CLEF
%token CM_T
%token CONSISTS
%token DURATION
%token EXTENDER
%token FONT
%token GROUPING
%token HEADER
%token IN_T
%token KEY
%token KEYSIGNATURE
%token LYRICS
%token MARK
%token MEASURES
%token MIDI
%token MM_T
%token MUSICAL_PITCH
%token NAME
%token NOTENAMES
%token NOTES
%token PAPER
%token PARTIAL
%token PENALTY
%token PROPERTY
%token PT_T
%token RELATIVE
%token REMOVE
%token REPEAT
%token SCM_T
%token SCMFILE
%token SCORE
%token SCRIPT
%token SHAPE
%token SKIP
%token SPANDYNAMIC
%token TEMPO
%token TIME_T
%token TIMES
%token TRANSLATOR
%token TRANSPOSE
%token TYPE
%token VERSION

/* escaped */
%token E_EXCLAMATION E_SMALLER E_BIGGER E_CHAR

%type <i>	dots
%token <i>	DIGIT
%token <pitch>	NOTENAME_PITCH
%token <pitch>	TONICNAME_PITCH
%token <pitch>	CHORDMODIFIER_PITCH
%token <id>	DURATION_IDENTIFIER
%token <id>	IDENTIFIER
%token <id>	NOTENAME_TABLE_IDENTIFIER
%token <id>	MUSIC_IDENTIFIER
%token <id>	POST_REQUEST_IDENTIFIER
%token <id>	SCRIPT_IDENTIFIER
%token <id>	COMMAND_IDENTIFIER
%token <id>	REAL_IDENTIFIER
%token <id>	STRING_IDENTIFIER
%token <id>	TRANS_IDENTIFIER
%token <id>	INT_IDENTIFIER
%token <id>	SCORE_IDENTIFIER
%token <id>	MIDI_IDENTIFIER
%token <id>	PAPER_IDENTIFIER
%token <real>	REAL
%token <string>	DURATION RESTNAME
%token <string>	STRING
%token <i>	UNSIGNED


%type <outputdef> output_def
%type <scope> 	mudela_header mudela_header_body
%type <i>	open_request_parens close_request_parens
%type <i>	sub_quotes sup_quotes
%type <music>	simple_element  request_chord command_element Simple_music  Composite_music 
%type <music>	Alternative_music Repeated_music
%type <i>	abbrev_type
%type <i>	int unsigned
%type <i>	script_dir
%type <i>	optional_modality
%type <id>	identifier_init simple_identifier_init block_identifier
%type <duration> steno_duration notemode_duration
%type <duration> entered_notemode_duration explicit_duration
%type <intvec>	intastint_list int_list

%type <pitch>   explicit_musical_pitch steno_musical_pitch musical_pitch absolute_musical_pitch
%type <pitch>   steno_tonic_pitch
%type <notereq>	steno_notepitch
%type <pitch_arr>	pitch_list
%type <music>	chord notemode_chord
%type <pitch_arr>	chord_additions chord_subtractions chord_notes
%type <pitch>	chord_addsub chord_note chord_inversion notemode_chord_inversion
%type <midi>	midi_block midi_body
%type <duration>	duration_length

%type <scalar>  scalar
%type <music>	Music  relative_music Sequential_music Simultaneous_music
%type <music>	property_def translator_change
%type <music_list> Music_list
%type <paper>	paper_block paper_def_body
%type <real>	real_expression real dimension
%type <request> abbrev_command_req
%type <request>	post_request structured_post_request
%type <request> command_req verbose_command_req
%type <request>	script_req  dynamic_req extender_req
%type <string>	string
%type <score>	score_block score_body
%type <intarr>	shape_array
%type <script>	script_definition script_body mudela_script gen_script_def
%type <textdef> text_def finger
%type <string>	script_abbreviation
%type <trans>	translator_spec translator_spec_body
%type <tempo> 	tempo_request
%type <notenametab> notenames_body notenames_block chordmodifiers_block

%expect 7


%left '-' '+'
%left '*' '/'
%left UNARY_MINUS

%%

mudela:	/* empty */
	| mudela toplevel_expression {}
	| mudela assignment { }
	| mudela error
	| mudela check_version { }
	;

toplevel_expression:
	notenames_block			{
		THIS->lexer_p_->set_notename_table ($1);
	}
	| chordmodifiers_block			{
		THIS->lexer_p_->set_chordmodifier_table ($1);
	}
	| mudela_header {
		delete header_global_p;
		header_global_p = $1;
	}
	| score_block {
		score_global_array.push ($1);
	}
	| paper_block {
		Identifier * id = new
			Paper_def_identifier ($1, PAPER_IDENTIFIER);
		THIS->lexer_p_->set_identifier ("$defaultpaper", id)
	}
	| midi_block {
		Identifier * id = new
			Midi_def_identifier ($1, MIDI_IDENTIFIER);
		THIS->lexer_p_->set_identifier ("$defaultmidi", id)
	}
	| embedded_scm { 
	}
	;

embedded_scm:
	SCMFILE STRING ';' {
		read_lily_scm_file (*$2);
		delete $2;
	}
	| SCM_T STRING ';' {
		gh_eval_str ($2->ch_l ());
		delete $2;
	};

check_version:
	VERSION STRING ';'		{
	 	Mudela_version ver (*$2);
		if (!((ver >= oldest_version) && (ver <= version))) {
			if (THIS->ignore_version_b_) {
				THIS->here_input ().error (_f ("incorrect mudela version: %s (%s, %s)", ver.str (), oldest_version.str (), version.str ()));
			} else {
				THIS->fatal_error_i_ = 1;
				THIS->parser_error (_f ("incorrect mudela version: %s (%s, %s)", ver.str (), oldest_version.str (), version.str ()));
			}
		}
	}
	;


chordmodifiers_block:
	CHORDMODIFIERS '{' notenames_body '}'  {  $$ = $3; }
	;


notenames_block:
	NOTENAMES '{' notenames_body '}'  {  $$ = $3; }
	;



notenames_body:
	/**/	{
		$$ = new Notename_table;
	}
	| NOTENAME_TABLE_IDENTIFIER	{
		$$ = $1-> access_content_Notename_table(true);
	}
	| notenames_body STRING '=' explicit_musical_pitch {
		(*$$)[*$2] = *$4;

		delete $4;
		delete $2;
	}
	;

mudela_header_body:
	{
		$$ = new Scope;
		THIS->lexer_p_-> scope_l_arr_.push ($$);
	}
	| mudela_header_body assignment ';' { 

	}
	;

mudela_header:
	HEADER '{' mudela_header_body '}'	{
		$$ = $3;
		THIS->lexer_p_-> scope_l_arr_.pop ();		
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
	    THIS->lexer_p_->set_identifier (*$1, $4);
	    $4->init_b_ = THIS->init_parse_b_;
	    $4->set_spot (THIS->pop_spot ());
	}
	;


simple_identifier_init: identifier_init
	;

identifier_init:
	block_identifier
	;

block_identifier:
	score_block {
		$$ = new Score_identifier ($1, SCORE_IDENTIFIER);

	}
	| chordmodifiers_block {
		$$ = new Notename_table_identifier ($1, NOTENAME_TABLE_IDENTIFIER);
	}
	| notenames_block {
		$$ = new Notename_table_identifier ($1, NOTENAME_TABLE_IDENTIFIER);
	}
	| paper_block {
		$$ = new Paper_def_identifier ($1, PAPER_IDENTIFIER);
	}
	| midi_block {
		$$ = new Midi_def_identifier ($1, MIDI_IDENTIFIER);

	}
	| translator_spec {
		$$ = new Translator_identifier ($1, TRANS_IDENTIFIER);
	}
	| Music  {
		$$ = new Music_identifier ($1, MUSIC_IDENTIFIER);
	}

	| post_request {
		$$ = new Request_identifier ($1, POST_REQUEST_IDENTIFIER);
	}
	| explicit_duration {
		$$ = new Duration_identifier ($1, DURATION_IDENTIFIER);
	}
	| real {
		$$ = new Real_identifier (new Real ($1), REAL_IDENTIFIER);
	}
	| string {
		$$ = new String_identifier ($1, STRING_IDENTIFIER);
	}
	| int	{
		$$ = new int_identifier (new int ($1), INT_IDENTIFIER);
	}
	| script_definition {
		$$ = new General_script_def_identifier ($1, SCRIPT_IDENTIFIER);

	}
	;

translator_spec:
	TRANSLATOR '{' translator_spec_body '}'
		{ $$ = $3; }
	;

translator_spec_body:
	TRANS_IDENTIFIER	{
		$$ = $1->access_content_Translator (true);
		$$-> set_spot (THIS->here_input ());
	}
	| TYPE STRING ';'	{
		Translator* t = get_translator_l (*$2);
		Translator_group * tg = dynamic_cast<Translator_group*> (t);

		if (!tg)
			THIS->parser_error (_("Need a translator group for a context"));
		
		t = t->clone ();
		t->set_spot (THIS->here_input ());
		$$ = t;
		delete $2;
	}
	| translator_spec_body STRING '=' simple_identifier_init ';'	{ 
		Identifier* id = $4;
		String_identifier *s = dynamic_cast<String_identifier*> (id);
		Real_identifier *r= dynamic_cast<Real_identifier*>(id);
		int_identifier *i = dynamic_cast<int_identifier*> (id);
	
		String str;
		if (s) str = *s->access_content_String (false); 
		if (i) str = to_str (*i->access_content_int (false));
		if (r) str = to_str (*r->access_content_Real (false));
		if (!s && !i && !r)
			THIS->parser_error (_("Wrong type for property value"));

		delete $4;
		$$->set_property (*$2, str);
	}
	| translator_spec_body NAME STRING ';' {
		$$->type_str_ = *$3;
		delete $3;
	}
	| translator_spec_body CONSISTS STRING ';' {
		dynamic_cast<Translator_group*> ($$)-> set_element (*$3, true);
		delete $3;
	}
	| translator_spec_body ACCEPTS STRING ';' {
		dynamic_cast<Translator_group*> ($$)-> set_acceptor (*$3, true);
		delete $3;
	}
	| translator_spec_body REMOVE STRING ';' {
		dynamic_cast<Translator_group*> ($$)-> set_element (*$3, false);
		delete $3;
	}
	;

/*
	SCORE
*/
score_block:
	SCORE { THIS->remember_spot ();
	}
	/*cont*/ '{' score_body '}' 	{
		$$ = $4;
		$$->set_spot (THIS->pop_spot ());
		if (!$$->def_p_arr_.size ())
			$$->add_output (THIS->default_paper_p ());

	}
	;

score_body:		{
		$$ = new Score;
	}
	| SCORE_IDENTIFIER {
		$$ = $1->access_content_Score (true);
	}
	| score_body mudela_header 	{
		$$->header_p_ = $2;
	}
	| score_body Music	{
		if ($$->music_p_)
			$2->warning (_ ("More than one music block"));	
		$$->music_p_ = $2;
	}
	| score_body output_def {
		$$->add_output ($2);
	}
	| score_body error {

	}
	;

output_def:
	paper_block {
		$$ = $1;
	}
	|  midi_block		{
		$$= $1;
	}
	;

intastint_list:
	/* */	{ $$ =new Array<int>; }
	| intastint_list int '*' int	{
		$$->push ($2); $$->push ($4);
	}
	| intastint_list int	{
		$$->push ($2); $$->push (1);
	}
	;	


/*
	PAPER
*/
paper_block:
	PAPER '{' paper_def_body '}' 	{ 
		$$ = $3;
		THIS-> lexer_p_->scope_l_arr_.pop ();
	}
	;

optional_semicolon:
	/* empty */
	| ';'
	;

optional_dot:
	/* empty */
	| '.'
	;

paper_def_body:
	/* empty */		 	{
		Paper_def *p = THIS->default_paper_p ();
		THIS-> lexer_p_-> scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| PAPER_IDENTIFIER optional_semicolon	{
		Paper_def *p = $1->access_content_Paper_def (true);
		THIS->lexer_p_->scope_l_arr_.push (p->scope_p_);
		$$ = p;
	}
	| paper_def_body int '=' FONT STRING		{ // ugh, what a syntax
		Lookup * l = new Lookup;
		l->font_name_ = *$5;
		delete $5;
		$$->set_lookup ($2, l);
	}
	| paper_def_body assignment ';' {

	}
	| paper_def_body translator_spec {
		$$->assign_translator ($2);
	}
	| paper_def_body SHAPE '=' shape_array ';' {
		$$->shape_int_a_ = *$4;
		delete $4;
	}
	| paper_def_body error {

	}
	;


real:
	real_expression  	{ $$ = $1; }
	;


dimension:
	REAL CM_T	{
		$$ = $1 CM;
	}
	| REAL PT_T	{
		$$ = $1 PT;
	}
	| REAL IN_T	{
		$$ = $1 INCH;
	}
	| REAL MM_T	{
		$$ = $1 MM;
	}
	;

real_expression:
	REAL		{
		$$ = $1;
	}
	| dimension
	| REAL_IDENTIFIER		{
		$$= *$1->access_content_Real (false);
	}
	| '-'  real_expression %prec UNARY_MINUS {
		$$ = -$2;
	}
	| real_expression '*' real_expression {
		$$ = $1 * $3;
	}
	| real_expression '/' real_expression {
		$$ = $1 / $3;
	}
	| real_expression '+' real_expression {
		$$ = $1  + $3;
	}
	| real_expression '-' real_expression {
		$$ = $1 - $3;
	}
	| '(' real_expression ')'	{
		$$ = $2;
	}
	;
		

shape_array:
	/* empty */ {
		$$ = new Array<Interval>;
	}
	| shape_array real real {
		$$->push(Interval($2, $2 + $3));
	};

/*
	MIDI
*/
midi_block:
	MIDI

	'{' midi_body '}' 	{ $$ = $3; }
	;

midi_body: /* empty */ 		{
		$$ = THIS->default_midi_p ();
	}
	| MIDI_IDENTIFIER	{
		$$ = $1-> access_content_Midi_def (true);
	}
	| midi_body translator_spec	{
		$$-> assign_translator ($2);
	}
	| midi_body tempo_request ';' {
		$$->set_tempo ($2->dur_.length (), $2->metronome_i_);
		delete $2;
	}
	| midi_body error {

	}
	;

tempo_request:
	TEMPO steno_duration '=' unsigned	{
		$$ = new Tempo_req;
		$$->dur_ = *$2;
		delete $2;
		$$-> metronome_i_ = $4;
	}
	;

Music_list: /* empty */ {
		$$ = new Music_list;
	}
	| Music_list Music {
		$$->add_music ($2);
	}
	| Music_list error {
	}
	;


Music:
	Simple_music
	| Composite_music
	;

Alternative_music: {
		Music_list* m = new Music_list;
		$$ = new Sequential_music (m);
	}
	| ALTERNATIVE Simultaneous_music {
		$$ = $2;
	}
	| ALTERNATIVE Sequential_music {
		$$ = $2;
	}
	;

Repeated_music: REPEAT unsigned Music Alternative_music	{
		Music_sequence* m = dynamic_cast <Music_sequence*> ($4);
		assert (m);
		$$ = new Repeated_music ($3, $2 >? 1, m);
	}
	;

Sequential_music: '{' Music_list '}'		{
		$$ = new Sequential_music ($2);
	}
	;

Simultaneous_music: '<' Music_list '>'	{
		$$ = new Simultaneous_music ($2);
	}
	;

Simple_music:
	request_chord		{ $$ = $1; }
	| MUSIC_IDENTIFIER { $$ = $1->access_content_Music (true); }
	| property_def
	| translator_change
	| Simple_music '*' unsigned '/' unsigned 	{
		/* urg */
		$$ = new Compressed_music ($3, $5, $1);
	}
	| Simple_music '*' unsigned		 {
		$$ = new Compressed_music ($3, 1, $1);
	}
	;


Composite_music:
	TYPE STRING Music	{
		$$ = $3;
		$$->translator_type_str_ = *$2;
		delete $2;
	}
	| TYPE STRING '=' STRING Music {
		$$ = $5;
		$$->translator_type_str_ = *$2;
		$$->translator_id_str_ = *$4;
		delete $2;
		delete $4;
	}
	| TIMES {
		THIS->remember_spot ();
	}
	/* CONTINUED */ 
		unsigned '/' unsigned Music 	

	{
		$$ = new Compressed_music ($3, $5, $6);
		$$->set_spot (THIS->pop_spot ());
	}
	| Repeated_music		{ $$ = $1; }
	| Simultaneous_music		{ $$ = $1; }
	| Sequential_music		{ $$ = $1; }
	| TRANSPOSE musical_pitch Music {
		$$ = new Transposed_music ($3, *$2);
		delete $2;
	}
	| TRANSPOSE steno_tonic_pitch Music {
		$$ = new Transposed_music ($3, *$2);
		delete $2;
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
		  $$ = $3;
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
	;

relative_music:
	RELATIVE absolute_musical_pitch Music {
		$$ = new Relative_octave_music ($3, *$2);
		delete $2;
	}
	;

translator_change:
	TRANSLATOR STRING '=' STRING  {
		Change_translator * t = new Change_translator;
		t-> change_to_type_str_ = *$2;
		t-> change_to_id_str_ = *$4;

		$$ = t;
		$$->set_spot (THIS->here_input ());
		delete $2;
		delete $4;
	}
	;

property_def:
	PROPERTY STRING '.' STRING '=' scalar	{
		Translation_property *t = new Translation_property;
		t-> translator_type_str_ = *$2;
		t-> var_str_ = *$4;
		t-> value_ = *$6;
		$$ = t;
		$$->set_spot (THIS->here_input ());
		delete $2;
		delete $4;
		delete $6;
	}
	;

scalar:
	STRING		{ $$ = new Scalar (*$1); delete $1; }
	| int		{ $$ = new Scalar ($1); }
	;


request_chord:
	pre_requests simple_element post_requests	{
	 	THIS->add_requests ((Simultaneous_music*)$2);//ugh
 		$$ = $2;
	}
	| command_element
	;

command_element:
	command_req {
		$$ = new Request_chord;
		$$-> set_spot (THIS->here_input ());
		$1-> set_spot (THIS->here_input ());
		((Simultaneous_music*)$$) ->add_music ($1);//ugh
	}
	;

command_req:
	abbrev_command_req
	| verbose_command_req ';'	{ $$ = $1; }
	;

abbrev_command_req:
	'|'				{
		$$ = new Barcheck_req;
	}
	| COMMAND_IDENTIFIER	{
		$$ = $1->access_content_Request (true);
	}
	| '~'	{
		$$ = new Tie_req;
	}
	| '['		{
		Beam_req*b= new Beam_req;
		b->spantype_ = START;
		$$ =b;
	}
	| ']'		{
		Beam_req*b= new Beam_req;
		b->spantype_ = STOP;
		$$ = b;
	}
	;


verbose_command_req:
	BAR STRING 			{
		$$ = new Bar_req (*$2);
		delete $2;
	}
	| MARK STRING {
		$$ = new Mark_req (*$2);
		delete $2;
	}
	| MARK unsigned {
		$$ = new Mark_req (to_str ($2));
	}
	| TIME_T unsigned '/' unsigned 	{
		Time_signature_change_req *m = new Time_signature_change_req;
		m->beats_i_ = $2;
		m->one_beat_i_=$4;
		$$ = m;
	}
	| PENALTY '=' int	{
		Break_req * b = new Break_req;
		b->penalty_i_ = $3;
		b-> set_spot (THIS->here_input ());
		$$ = b;
	}
	| SKIP duration_length {
		Skip_req * skip_p = new Skip_req;
		skip_p->duration_ = *$2;
		delete $2;
		$$ = skip_p;
	}
	| tempo_request {
		$$ = $1;
	}
	| CADENZA unsigned	{
		$$ = new Cadenza_req ($2);
	}
	| PARTIAL duration_length 	{
		$$ = new Partial_measure_req ($2->length ());
		delete $2;
	}
	| CLEF STRING {
		$$ = new Clef_change_req (*$2);
		delete $2;
	}
	| KEY NOTENAME_PITCH optional_modality	{
		Key_change_req *key_p= new Key_change_req;
		key_p->pitch_arr_.push(*$2);
		key_p->ordinary_key_b_ = true;
		key_p->modality_i_ = $3;
		$$ = key_p;
		delete $2;
	}
	| KEYSIGNATURE pitch_list 	{
		Key_change_req *key_p= new Key_change_req;
		key_p->pitch_arr_ = *$2;
		key_p->ordinary_key_b_ = false;
		$$ = key_p;
		delete $2;
	}
	| GROUPING intastint_list {
		$$ = get_grouping_req (*$2); delete $2;
	}
	;

post_requests:
	{
		/* something silly happened.  Junk this stuff*/
		if (!THIS->post_reqs.empty ())
		{
			warning ("Junking post-requests");
			THIS->post_reqs.clear ();
		}
	}
	| post_requests structured_post_request {
		$2->set_spot (THIS->here_input ());
		THIS->post_reqs.push ($2);
	}
	| post_requests close_request_parens	{
		Array<Request*>& r = *THIS->get_parens_request ($2);
		for (int i = 0; i < r.size (); i++ )
			r[i]->set_spot (THIS->here_input ());
		THIS->post_reqs.concat (r);
		delete &r;
	}
	;

structured_post_request:
	script_req
	| post_request
	;

post_request:
	POST_REQUEST_IDENTIFIER	{
		$$ = (Request*)$1->access_content_Request (true);
	}
	| dynamic_req {
		$$ = $1;
	}
	| abbrev_type	{
		Abbreviation_req* a = new Abbreviation_req;
		a->type_i_ = $1;
		$$ = a;
	}
	| extender_req {
		$$ = $1;
	}
	;

optional_modality:
	/* empty */	{
		$$ = 0;
	}
	| int	{
		$$ = $1;
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

steno_musical_pitch:
	NOTENAME_PITCH	{
		$$ = $1;
	}
	| NOTENAME_PITCH sup_quotes 	{
		$$ = $1;
		$$->octave_i_ +=  $2;
	}
	| NOTENAME_PITCH sub_quotes	 {
		$$ = $1;
		$$->octave_i_ += - $2;
	}
	;

steno_tonic_pitch:
	TONICNAME_PITCH	{
		$$ = $1;
	}
	| TONICNAME_PITCH sup_quotes 	{
		$$ = $1;
		$$->octave_i_ +=  $2;
	}
	| TONICNAME_PITCH sub_quotes	 {
		$$ = $1;
		$$->octave_i_ += - $2;
	}
	;

explicit_musical_pitch:
	MUSICAL_PITCH '{' int_list '}'	{/* ugh */
		Array<int> &a = *$3;
		ARRAY_SIZE(a,3);
		$$ = new Musical_pitch;
		$$->octave_i_ = a[0];
		$$->notename_i_ = a[1];
		$$->accidental_i_ = a[2];
		delete &a;
	}
	;

musical_pitch:
	steno_musical_pitch
	| explicit_musical_pitch
	;

steno_notepitch:
	musical_pitch	{
		$$ = new Note_req;
		
		$$->pitch_ = *$1;
		delete $1;
	}
	| steno_notepitch  '!' 		{
		$$->forceacc_b_ = ! $$->forceacc_b_;
	}
	| steno_notepitch  '?' 		{
		$$->forceacc_b_ = ! $$->forceacc_b_;
		$$->cautionary_b_ = ! $$->cautionary_b_;
	}
	;


explicit_duration:
	DURATION '{' int_list '}'	{
		$$ = new Duration;
		Array<int> &a = *$3;
		ARRAY_SIZE(a,2);
			
		$$-> durlog_i_ = a[0];
		$$-> dots_i_ = a[1];

		delete &a;		
	}
	;

extender_req:
	EXTENDER {
		if (!THIS->lexer_p_->lyric_state_b ())
			THIS->parser_error (_ ("have to be in Lyric mode for lyrics"));
		Extender_req * e_p = new Extender_req;
		e_p->spantype_ = START;
		$$ = e_p;
		THIS->extender_req = e_p;
	};

dynamic_req:
	ABSDYNAMIC '{' unsigned '}'	{
		Absolute_dynamic_req *ad_p = new Absolute_dynamic_req;
		ad_p ->loudness_ = (Dynamic_req::Loudness)$3;
		$$ =ad_p;
	}
	| SPANDYNAMIC '{' int int '}' {
		Span_dynamic_req * sp_p = new Span_dynamic_req;
		sp_p->spantype_ = (Direction)$4;
		sp_p-> dynamic_dir_  = (Direction)$3;
		$$ = sp_p;
	}
	;



close_request_parens:
	'('	{
		$$='(';
	}
	| E_SMALLER {
		$$ = '<';
	}
	| E_BIGGER {
		$$ = '>';
	}
	;



open_request_parens:
	E_EXCLAMATION 	{
		$$ = '!';
	}
	| ')'	{
		$$=')';
	}
	;



script_definition:
	SCRIPT '{' script_body '}' 	{ $$ = $3; }
	;

script_body:
	STRING int int int int int		{
		Script_def *s = new Script_def;
		s->set_from_input (*$1,$2, $3,$4,$5, $6);
		$$  = s;
		delete $1;
	}
	;

script_req:
	script_dir gen_script_def	{
		Musical_script_req *m = new Musical_script_req;
		$$ = m;
		m->scriptdef_p_ = $2;
		m->set_spot (THIS->here_input ());
		if (!m->dir_)
		  m->dir_  = (Direction)$1;
	}
	;

gen_script_def:
	text_def	{ 
		$$ = $1;
		((Text_def*) $$)->align_dir_ = LEFT; /* UGH */
	}
	| mudela_script	{ 
		$$ = $1;
		$$-> set_spot (THIS->here_input ());
	}
	| finger {
		$$ = $1;
		((Text_def*)$$)->align_dir_ = RIGHT; /* UGH */
	}
	;

text_def:
	string {
		Text_def *t  = new Text_def;
		$$ = t;
		t->text_str_ = *$1;
		delete $1;
		$$->set_spot (THIS->here_input ());
	}
	;

finger:
	 DIGIT {
		Text_def* t  = new Text_def;
		$$ = t;
		t->text_str_ = to_str ($1);
		t->style_str_ = "finger";
		$$->set_spot (THIS->here_input ());
	}
	;

script_abbreviation:
	'^'		{ $$ = get_scriptdef ('^'); }
	| '+'		{ $$ = get_scriptdef ('+'); }
	| '-'		{ $$ = get_scriptdef ('-'); }
 	| '|'		{ $$ = get_scriptdef ('|'); }
	| 'o'		{ $$ = get_scriptdef ('o'); }
	| '>'		{ $$ = get_scriptdef ('>'); }
	| '.' 		{
		$$ = get_scriptdef ('.');
	}
	;

mudela_script:
	SCRIPT_IDENTIFIER		{ $$ = $1->access_content_General_script_def (true); }
	| script_definition		{ $$ = $1; }
	| script_abbreviation		{
		$$ = THIS->lexer_p_->lookup_identifier (*$1)->access_content_General_script_def (true);
		delete $1;
	}
	;

script_dir:
	'_'	{ $$ = -1; }
	| '^'	{ $$ = 1; }
	| '-'	{ $$ = 0; }
	;

pre_requests:
	{
		if (THIS->extender_req)
		  {
		    Extender_req * e_p = new Extender_req;
		    e_p->spantype_ = STOP;
		    THIS->pre_reqs.push (e_p);
		    THIS->extender_req = 0;
		  }
			
	}
	| pre_requests open_request_parens {

		Array<Request*>& r = *THIS->get_parens_request ($2);
		for (int i = 0; i < r.size (); i++ )
			r[i]->set_spot (THIS->here_input ());
		THIS->pre_reqs.concat (r);
		delete &r;
	}
	;

absolute_musical_pitch:
	steno_musical_pitch	{
		$$ = $1;
	}
	;

duration_length:
	steno_duration {
		$$ = $1;
	}
	| duration_length '*' unsigned {
		$$->plet_.iso_i_ *= $3;
	}
	| duration_length '/' unsigned {
		$$->plet_.type_i_ *= $3;
	}
	;

dots:
	'.'		{ $$ = 1; }
	| dots '.'	{ $$ ++; }
	;

entered_notemode_duration:
	/* */		{
		$$ = new Duration (THIS->default_duration_);
	}
	| dots		{
		$$ = new Duration (THIS->default_duration_);
		$$->dots_i_  = $1;
	}
	| steno_duration	{
		THIS->set_last_duration ($1);
	}
	;

notemode_duration:
	entered_notemode_duration {
		$$ = $1;
	}
	;

steno_duration:
	unsigned		{
		$$ = new Duration;
		if (!Duration::duration_type_b ($1))
			THIS->parser_error (_f ("not a duration: %d", $1));
		else {
			$$->durlog_i_ = Duration_convert::i2_type ($1);
		     }
	}
	| DURATION_IDENTIFIER	{
		$$ = $1->access_content_Duration (true);
	}
	| steno_duration '.' 	{
		$$->dots_i_ ++;
	}
	;


abbrev_type: 
	':'	{
		$$ =0;
	}
	| ':' unsigned {
		if (!Duration::duration_type_b ($2))
			THIS->parser_error (_f ("not a duration: %d", $2));
		else if ($2 < 8)
			THIS->parser_error (_ ("can't abbreviate"));
		$$ = $2;
	}
	;



simple_element:
	steno_notepitch notemode_duration  {
		if (!THIS->lexer_p_->note_state_b ())
			THIS->parser_error (_ ("have to be in Note mode for notes"));
		$1->duration_ = *$2;
		$$ = THIS->get_note_element ($1, $2);
	}
	| RESTNAME notemode_duration		{
		$$ = THIS->get_rest_element (*$1, $2);
		delete $1;  // delete notename
	}
	| MEASURES notemode_duration  	{
		Multi_measure_rest_req* m = new Multi_measure_rest_req;
		m->duration_ = *$2;
		delete $2;

		Simultaneous_music*velt_p = new Request_chord;
		velt_p->set_spot (THIS->here_input ());
		velt_p->add_music (m);
		$$ = velt_p;
	}
	| STRING notemode_duration 			{
		if (!THIS->lexer_p_->lyric_state_b ())
			THIS->parser_error (_ ("have to be in Lyric mode for lyrics"));
		$$ = THIS->get_word_element (*$1, $2);
		delete $1;
	}
	| chord {
		if (!THIS->lexer_p_->chord_state_b ())
			THIS->parser_error (_ ("have to be in Chord mode for chords"));
		$$ = $1;
	}
	| '@' notemode_chord '@' {
		if (!THIS->lexer_p_->note_state_b ())
			THIS->parser_error (_ ("have to be in Note mode for @chords"));
		$$ = $2;
	}
	;

chord:
	steno_tonic_pitch notemode_duration chord_additions chord_subtractions chord_inversion {
                $$ = THIS->get_chord (*$1, $3, $4, $5, *$2);
        };

notemode_chord:
	steno_musical_pitch notemode_duration chord_additions chord_subtractions notemode_chord_inversion {
                $$ = THIS->get_chord (*$1, $3, $4, $5, *$2);
        };

chord_additions: 
	{
		$$ = new Array<Musical_pitch>;
	} 
	| '-' chord_notes {
		$$ = $2;
	}
	;

chord_notes:
	{
		$$ = new Array<Musical_pitch>;
	}
	| chord_notes chord_addsub {
		$$ = $1;
		$$->push (*$2);
	}
	;

chord_addsub:
	chord_note optional_dot
	| CHORDMODIFIER_PITCH optional_dot
	;

chord_inversion:
	{
		$$ = 0;
	}
	| '/' steno_tonic_pitch {
		$$ = $2
	}
	;

notemode_chord_inversion:
	{
		$$ = 0;
	}
	| '/' steno_musical_pitch {
		$$ = $2
	}
	;

chord_note:
	unsigned {
		$$ = new Musical_pitch;
		$$->notename_i_ = ($1 - 1) % 7;
		$$->octave_i_ = $1 > 7 ? 1 : 0;
		$$->accidental_i_ = 0;
        } 
	| unsigned '+' {
		$$ = new Musical_pitch;
		$$->notename_i_ = ($1 - 1) % 7;
		$$->octave_i_ = $1 > 7 ? 1 : 0;
		$$->accidental_i_ = 1;
	}
	| unsigned '-' {
		$$ = new Musical_pitch;
		$$->notename_i_ = ($1 - 1) % 7;
		$$->octave_i_ = $1 > 7 ? 1 : 0;
		$$->accidental_i_ = -1;
	}
        ;

chord_subtractions: 
	{
		$$ = new Array<Musical_pitch>;
	} 
	| '^' chord_notes {
		$$ = $2;
	}
	;

/*
	UTILITIES
 */
pitch_list:			{
		$$ = new Array<Musical_pitch>;
	}
	| pitch_list musical_pitch	{
		$$->push (*$2);
		delete $2;
	}
	;


int_list:
	/**/			{
		$$ = new Array<int>
	}
	| int_list int 		{
		$$->push ($2);		
	}
	;

unsigned:
	UNSIGNED	{
		$$ = $1;
	}
	| DIGIT {
		$$ = $1;
	};

int:
	unsigned {
		$$ = $1;
	}
	| '-' unsigned {
		$$ = -$2;
	}
	| INT_IDENTIFIER	{
		$$ = *$1->access_content_int (false);
	}
	;


string:
	STRING		{
		$$ = $1;
	}
	| STRING_IDENTIFIER	{
		$$ = $1->access_content_String (true);
	}
	| string '+' string {
		*$$ += *$3;
		delete $3;
	}
	;






%%

void
My_lily_parser::set_yydebug (bool b)
{
#ifdef YYDEBUG
	yydebug = b;
#endif
}
void
My_lily_parser::do_yyparse ()
{
	yyparse ((void*)this);
}


