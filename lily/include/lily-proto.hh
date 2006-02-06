/*
  lily-proto.hh -- declare class names.

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef LILY_PROTO_HH
#define LILY_PROTO_HH
#include "flower-proto.hh"

class All_font_metrics;
class Audio_column;
class Audio_dynamic;
class Audio_element;
class Audio_instrument;
class Audio_item;
class Audio_key;
class Audio_note;
class Audio_piano_pedal;
class Audio_staff;
class Audio_tempo;
class Audio_text;
class Audio_tie;
class Audio_time_signature;
class Auto_change_iterator;
class Auto_change_music;
class Axis_group_engraver;
class Bar_engraver;
class Bar_req_collect_engraver;
class Beaming_info_list;
class Bezier;
class Bezier_bow;
class Book;
class Box;
class Break_algorithm;
class Change_iterator;
class Change_translator;
class Chord_tremolo_iterator;
class Cluster_engraver;
class Column_x_positions;
class Context;
class Context_def;
class Context_specced_music;
class Engraver;
class Engraver;
class Engraver_group;
class Event;
class Event_chord;
class Event_chord_iterator;
class Folded_repeat_iterator;
class Font_metric;
class Font_size_engraver;
class Global_context;
class Gourlay_breaking;
class Grace_fixup;
class Grace_iterator;
class Grace_music;
class Grob;
class Grob_array;
class Grob_info;
class Hara_kiri_engraver;
class Hara_kiri_line_group_engraver;
class Includable_lexer;
class Input;
class Input_file_results;
class Item;
class Key_performer;
class Keyword_ent;
class Keyword_table;
class Ligature_bracket_engraver;
class Ligature_engraver;
class Lily_lexer;
class Lily_parser;
class Lilypond_context_key;
class Lilypond_grob_key;
class Line_group_engraver_group;
class Lookup;
class Lyric_combine_music;
class Lyric_combine_music_iterator;
class Lyric_engraver;
class Lyric_performer;
class Lyric_phrasing_engraver;
class Mensural_ligature_engraver;
class Midi_chunk;
class Midi_duration;
class Midi_dynamic;
class Midi_header;
class Midi_instrument;
class Midi_item;
class Midi_key;
class Midi_note;
class Midi_note_event;
class Midi_note_off;
class Midi_piano_pedal;
class Midi_stream;
class Midi_tempo;
class Midi_text;
class Midi_time_signature;
class Midi_track;
class Modified_font_metric;
class Moment;
class Music;
class Music_iterator;
class Music_list;
class Music_output;
class Music_sequence;
class Music_wrapper;
class Music_wrapper_iterator;
class Note_performer;
class Output_def;
class Object_key;
class Object_key_dumper;
class Object_key_undumper;
class Open_type_font;
class Output_property;
class Page;
class Pango_font;
class Paper_book;
class Paper_column;
class Paper_outputter;
class Paper_score;
class Performance;
class Performer;
class Performer_group;
class Piano_bar_engraver;
class Pitch;
class Pitch_squash_engraver;
class Property_iterator;
class Rational;
class Relative_octave_music;
class Repeated_music;
class Scheme_hash_table;
class Score;
class Score_context;
class Score_engraver;
class Score_performer;
class Sequential_music;
class Sequential_music_iterator;
class Simple_music_iterator;
class Simple_spacer;
class Simple_spacer_wrapper;
class Simultaneous_music;
class Simultaneous_music_iterator;
class Skyline_entry;
class Slur_configuration;
class Slur_score_state;
class Span_score_bar_engraver;
class Spanner;
class Staff_group_bar_engraver;
class Staff_performer;
class Stencil;
class Swallow_engraver;
class Swallow_performer;
class System;
class Tempo_performer;
class Tex_font_metric;
class Tie;
class Tie_details;
class Tie_configuration;
class Tie_formatting_problem;
class Tie_performer;
class Time_scaled_music;
class Time_scaled_music_iterator;
class Time_signature_performer;
class Timing_engraver;
class Timing_translator;
class Translation_property;
class Translator;
class Translator_change;
class Translator_group;
class Transposed_music;
class Type_swallow_translator;
class yyFlexLexer;

typedef void (*Engraver_void_function_engraver_grob_info) (Engraver *, Grob_info);
typedef void (*Translator_void_method_ptr) (Translator *);


/* FIXME: when Link_array is dropped, do grand s/r to vector<TYPE*>.  */
#if STD_VECTOR

#include "std-vector.hh"

#define Link_array__char_ std::vector<char*>
#define Link_array__Grob_ std::vector<Grob*>
#define Link_array__Accidental_placement_entry_ std::vector<Accidental_placement_entry*>
#define Link_array__Audio_item_ std::vector<Audio_item*>
#define Link_array__Audio_note_ std::vector<Audio_note*>
#define Link_array__Audio_piano_pedal_ std::vector<Audio_piano_pedal*>
#define Link_array__Audio_staff_ std::vector<Audio_staff*>
#define Link_array__Bracket_nesting_node_ std::vector<Bracket_nesting_node*>
#define Link_array__Context_ std::vector<Context*>
#define Link_array__Context_def_ std::vector<Context_def*>
#define Link_array__Grob_ std::vector<Grob*>
#define Link_array__Item_ std::vector<Item*>
#define Link_array__Music_ std::vector<Music*>
#define Link_array__Note_column_ std::vector<Note_column*>
#define Link_array__Output_def_ std::vector<Output_def*>
#define Link_array__Slur_configuration_ std::vector<Slur_configuration*>
#define Link_array__Source_file_ std::vector<Source_file*>
#define Link_array__Spanner_ std::vector<Spanner*>
#define Link_array__Tie_configuration_ std::vector<Tie_configuration*>
#else /* !STD_VECTOR */
#define Link_array__char_ Link_array<char>
#define Link_array__Grob_ Link_array<Grob>
#define Link_array__Accidental_placement_entry_ Link_array<Accidental_placement_entry>
#define Link_array__Audio_item_ Link_array<Audio_item>
#define Link_array__Audio_note_ Link_array<Audio_note>
#define Link_array__Audio_piano_pedal_ Link_array<Audio_piano_pedal>
#define Link_array__Audio_staff_ Link_array<Audio_staff>
#define Link_array__Bracket_nesting_node_ Link_array<Bracket_nesting_node>
#define Link_array__Context_ Link_array<Context>
#define Link_array__Context_def_ Link_array<Context_def>
#define Link_array__Grob_ Link_array<Grob>
#define Link_array__Item_ Link_array<Item>
#define Link_array__Music_ Link_array<Music>
#define Link_array__Note_column_ Link_array<Note_column>
#define Link_array__Output_def_ Link_array<Output_def>
#define Link_array__Slur_configuration_ Link_array<Slur_configuration>
#define Link_array__Source_file_ Link_array<Source_file>
#define Link_array__Spanner_ Link_array<Spanner>
#define Link_array__Tie_configuration_ Link_array<Tie_configuration>
#endif /* !STD_VECTOR */


#endif /* LILY_PROTO_HH */
