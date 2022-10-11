/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef LILY_PROTO_HH
#define LILY_PROTO_HH

#include "flower-proto.hh"

class All_font_metrics;
class Audio_column;
class Audio_control_change;
class Audio_element;
class Audio_instrument;
class Audio_item;
class Audio_key;
class Audio_note;
class Audio_piano_pedal;
class Audio_staff;
class Audio_tempo;
class Audio_text;
class Audio_time_signature;
class Axis_group_engraver;
class Bar_engraver;
class Beaming_pattern;
class Beam_scoring_problem;
class Beam_configuration;
class Beam_quant_parameters;
class Bezier;
class Book;
class Box;
class Change_iterator;
class Column_x_positions;
class Context;
class Context_def;
class Context_mod;
class Context_specced_music;
class Dispatcher;
class Dot_column;
class Dot_configuration;
class Dot_formatting_problem;
class Engraver;
class Engraver_group;
class Event;
class Event_chord;
class Event_chord_iterator;
class Event_iterator;
class Font_metric;
class Font_size_engraver;
class Global_context;
class Grace_fixup;
class Grace_iterator;
class Grace_music;
class Grob;
class Grob_array;
class Grob_properties;
class Includable_lexer;
class Input;
class Item;
class Key_performer;
class Ligature_bracket_engraver;
class Lazy_skyline_pair;
class Ligature_engraver;
class Lily_lexer;
class Lily_parser;
class Listener;
class Lyric_combine_music;
class Lyric_combine_music_iterator;
class Lyric_engraver;
class Lyric_performer;
class Mensural_ligature_engraver;
class Midi_chunk;
class Midi_control_change;
class Midi_control_change_announcer;
class Midi_duration;
class Midi_event;
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
class Music_output;
class Music_sequence;
class Music_wrapper;
class Music_wrapper_iterator;
class Note_performer;
class Output_def;
class Open_type_font;
class Output_property;
class Page_breaking;
class Pango_font;
class Paper_book;
class Paper_column;
class Paper_outputter;
class Paper_score;
class Performance;
class Performer;
class Performer_group;
class Pitch;
class Pitch_squash_engraver;
class Prob;
class Property_iterator;
class Relative_octave_music;
class Rhythmic_music_iterator;
class Scale;
class Scheme_hash_table;
class Scheme_engraver;
class Scm_module;
class Scm_variable;
class Score;
class Score_engraver;
class Score_performer;
class Simple_music_iterator;
class Simple_spacer;
class Simultaneous_music;
class Simultaneous_music_iterator;
class Skyline;
class Skyline_pair;
class Slur_configuration;
class Slur_score_state;
class Source_file;
class Sources;
class Spacing_options;
class Spanner;
class Staff_performer;
class Stencil;
class Stream_event;
class System;
class Tempo_performer;
class Tie;
class Tie_details;
class Tie_configuration;
class Tie_formatting_problem;
class Tie_performer;
class Time_signature_performer;
class Timing_translator;
class Transform;
class Translator;
class Translator_creator;
class Translator_group;

template <class>
class Grob_info_t;
using Grob_info = Grob_info_t<Grob>;

#endif /* LILY_PROTO_HH */
