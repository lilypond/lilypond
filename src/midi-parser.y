%{

#include <iostream.h>

#include "my-midi-lexer.hh"
#include "my-midi-parser.hh"
#include "midi-event.hh"
#include "midi-track.hh"
#include "midi-score.hh"

#ifndef NDEBUG
#define YYDEBUG 1
#endif

%}

%union {
    Byte byte;
    char c;
    int i;
    String* str_p;
    Request* request_p;
    Midi_event* midi_event_p;	// Voice_element* ?
    Midi_score* midi_score_p; 	// Input_score* ?
    Midi_track* midi_track_p;	// Input_music* ?
}

%token HEADER TRACK
%token SYSEX_EVENT1 SYSEX_EVENT2
%token META_EVENT
%token SEQUENCE TEXT COPYRIGHT TRACK_NAME INSTRUMENT_NAME LYRIC MARKER CUE_POINT
%token END_OF_TRACK TEMPO SMPTE_OFFSET TIME KEY SSME

%token<i> INT8 INT16 INT32 INT7_8UNSET INT7_8SET VARINT
%token<i> RUNNING_STATUS NOTE_OFF NOTE_ON PROGRAM_CHANGE
%token<str_p> DATA

%type <midi_score_p> header midi_score
%type <midi_track_p> track
%type <midi_event_p> event
%type <midi_event_p> the_event meta_event text_event midi_event sysex_event
%type <midi_event_p> running_status note_off note_on program_change

%%

midi:	/* empty */
	| midi midi_score {
		midi_parser_l_g->add_score( $2 );		
	}
	;

midi_score:
	header {
	}
	| midi_score track {
		$$->add_track( $2 );
	}
	;

header:	
	HEADER INT32 INT16 INT16 INT16 {
		$$ = new Midi_score( $3, $4, $5 );
	}
	;

track: 
	TRACK INT32 {
		$$ = new Midi_track;
	}
	| track event {
		$$->add_event( $2 );
	}
	;

event:	
	VARINT the_event {
	}
	;
	
the_event: 
	meta_event {
	}
	| midi_event {
	}
	| sysex_event {
	}
	;

meta_event:
	META_EVENT the_meta_event {
	};

the_meta_event:
	SEQUENCE INT16 {
	}
	| text_event DATA {
	}
	| END_OF_TRACK {
	}
	| TEMPO INT8 INT8 INT8 { 
	}
	| SMPTE_OFFSET INT8 INT8 INT8 INT8 INT8 { 
	}
	| TIME INT8 INT8 INT8 INT8 { 
	}
	| KEY INT8 INT8 { 
	}
	| SSME DATA {
	}
	;

text_event: 
	TEXT {
	}
	| COPYRIGHT {
	}
	| TRACK_NAME {
	}
	| INSTRUMENT_NAME {
	}
	| LYRIC {
	}
	| MARKER {
	}
	| CUE_POINT {
	}
	;

midi_event: 
	running_status {
	}
	| note_off {
	}
	| note_on {
	}
	| program_change {
	}
	;

running_status:
	RUNNING_STATUS {
	}
	;

note_off:
	NOTE_OFF INT8 INT8 {
	}
	;

note_on:
	NOTE_ON INT8 INT8 {
		int pitch_i = $2;
		// assuming key of C
		String notename_str = ( pitch_i % 12 )[ "ccddeffggaab" ];
		static int accidental_i_a[ 12 ] = { 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0 };
		int accidental_i = accidental_i_a[ pitch_i % 12 ];
		if ( accidental_i == 1 )
			notename_str += "is";
		cout << "note(" << pitch_i << "): " << notename_str << endl;
	}
	;

program_change:
	PROGRAM_CHANGE {
	}
	;

sysex_event:
	SYSEX_EVENT1 {
	}
	| SYSEX_EVENT2 {
	}
	;
