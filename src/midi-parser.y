%{

#include <iostream.h>

#include "proto.hh"         // ugh, these all for midi-main.hh 
#include "plist.hh"
#include "string.hh"
#include "sourcefile.hh"
#include "source.hh"
#include "midi-main.hh"    // *tors

#include "my-midi-lexer.hh"
#include "my-midi-parser.hh"
#include "moment.hh"
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
%token<i> RUNNING_STATUS DATA_ENTRY ALL_NOTES_OFF
%token<i> NOTE_OFF NOTE_ON 
%token<i> POLYPHONIC_AFTERTOUCH CONTROLMODE_CHANGE PROGRAM_CHANGE 
%token<i> CHANNEL_AFTERTOUCH PITCHWHEEL_RANGE
%token<str_p> DATA

%type <i> varint
%type <midi_score_p> header midi_score
%type <midi_track_p> track
%type <midi_event_p> event
%type <midi_event_p> the_event meta_event the_meta_event text_event midi_event sysex_event
%type <midi_event_p> running_status data_entry all_notes_off
%type <midi_event_p> note_off note_on
%type <midi_event_p> polyphonic_aftertouch controlmode_change program_change
%type <midi_event_p> channel_aftertouch pitchwheel_range

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
		midi_parser_l_g->set_division( $5 );
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
	varint the_event {
		if ( $2 && $2->mudela_str().length_i() ) {
			if ( ( $2->mudela_str()[ 0 ] >= 'a' ) 
				&& $2->mudela_str()[ 0 ] <= 'g' ) 
				qtor << $2->mudela_str() << " ";
			else
				vtor << $2->mudela_str() << " ";
		}
	}
	;
	
varint:
	VARINT {
		midi_parser_l_g->forward( $1 );
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
		$$ = $2;
	}
	|
	META_EVENT INT8 INT8 INT8 {
		$$ = 0;
	}
	;

the_meta_event:
	SEQUENCE INT16 {
	}
	| text_event DATA {
		$$ = 0;
		vtor << *$2 << endl;
		delete $2;
	}
	| END_OF_TRACK {
		$$ = 0;
	}
	| TEMPO INT8 INT8 INT8 { 
		$$ = new Midi_tempo( ( $2 << 16 ) + ( $3 << 8 ) + $4 );
		vtor << $$->mudela_str() << endl; // ?? waai not at event:
		midi_parser_l_g->set_tempo( ( $2 << 16 ) + ( $3 << 8 ) + $4 );
	}
	| SMPTE_OFFSET INT8 INT8 INT8 INT8 INT8 { 
		$$ = 0;
	}
	| TIME INT8 INT8 INT8 INT8 { 
		$$ = new Midi_time( $2, $3, $4, $5 );
		vtor << $$->mudela_str() << endl; // ?? waai not at event:
		midi_parser_l_g->set_time( $2, $3, $4, $5 );
	}
	| KEY INT8 INT8 { 
		$$ = new Midi_key( $2, $3 );
		midi_parser_l_g->set_key( $2, $3  );
	}
	| SSME DATA {
		$$ = 0;
		delete $2;
	}
	;

text_event: 
	TEXT {
		vtor << endl << "Text: ";
	}
	| COPYRIGHT {
		vtor << endl << "Copyright: ";
	}
	| TRACK_NAME {
		vtor << endl << "Track  name: ";
	}
	| INSTRUMENT_NAME {
		vtor << endl << "Instrument  name: ";
	}
	| LYRIC {
		vtor << endl << "Lyric: ";
	}
	| MARKER {
		vtor << endl << "Marker: ";
	}
	| CUE_POINT {
		vtor << endl << "Cue point: ";
	}
	;

midi_event: 
	running_status {
	}
	| data_entry {
	}
	| all_notes_off {
	}
	| note_off {
	}
	| note_on {
	}
	| polyphonic_aftertouch {
	}
	| controlmode_change {
	}
	| program_change {
	}
	| channel_aftertouch {
	}
	| pitchwheel_range {
	}
	;

running_status:
	RUNNING_STATUS INT8 { //INT8 {
		$$ = 0;
	}
	;

data_entry:
	DATA_ENTRY INT8 {
		$$ = 0;
	}
	;

all_notes_off:
	ALL_NOTES_OFF INT8 INT8 {
		$$ = 0;
	}
	;

note_off:
	NOTE_OFF INT8 INT8 {
		int i = $1;
		i = i & ~0x80;
		$$ = midi_parser_l_g->note_end_midi_event_p( $1 & ~0x80, $2, $3 );
	}
	;

note_on:
	NOTE_ON INT8 INT8 {
		int i = $1;
		i = i & ~0x90;
		$$ = 0;
		midi_parser_l_g->note_begin( $1 & ~0x90, $2, $3 );
	}
	;

polyphonic_aftertouch:
	POLYPHONIC_AFTERTOUCH INT8 INT8 {
		$$ = 0;
	}
	;

controlmode_change:
	CONTROLMODE_CHANGE INT8 INT8 {
		$$ = 0;
	}
	;

program_change:
	PROGRAM_CHANGE INT8 {
		$$ = 0;
	}
	;

channel_aftertouch:
	CHANNEL_AFTERTOUCH INT8 INT8 {
		$$ = 0;
	}
	;

pitchwheel_range:
	PITCHWHEEL_RANGE INT8 INT8 {
		$$ = 0;
	}
	;

sysex_event:
	SYSEX_EVENT1 DATA {
		$$ = 0;
	}
	| SYSEX_EVENT2 DATA { // INT8 ?
		$$ = 0;
	}
	;
