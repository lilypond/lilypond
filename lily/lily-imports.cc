/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2015 by David Kastrup <dak@gnu.org>

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

#include "lily-imports.hh"

namespace Guile_user {
  Scm_module module ("guile-user");

  Variable apply ("apply");
  Variable plus ("+");
  Variable make_module ("make-module");
#if GUILEV2
  Variable module_export_all_x ("module-export-all!");
#endif
  Variable module_export_x ("module-export!");
  Variable module_public_interface ("module-public-interface");
  Variable module_use_x ("module-use!");
  Variable symbol_p ("symbol?");
  Variable the_root_module ("the-root-module");
}

namespace Display {
  Scm_module module ("scm display-lily");

  Variable value_to_lily_string ("value->lily-string");
}

namespace Lily {
  Scm_module module ("lily");

  Variable all_music_font_encodings ("all-music-font-encodings");
  Variable alterations_in_key ("alterations-in-key");
  Variable backend_testing ("backend-testing");
  Variable base_length ("base-length");
  Variable beam_exceptions ("beam-exceptions");
  Variable beat_structure ("beat-structure");
  Variable calc_repeat_slash_count ("calc-repeat-slash-count");
  Variable car_less ("car<");
  Variable construct_chord_elements ("construct-chord-elements");
  Variable default_time_signature_settings ("default-time-signature-settings");
  Variable grob_compose_function ("grob::compose-function");
  Variable grob_offset_function ("grob::offset-function");
  Variable hash_table_to_alist ("hash-table->alist");
  Variable interpret_markup_list ("interpret-markup-list");
  Variable invalidate_alterations ("invalidate-alterations");
  Variable key_p ("key?");
  Variable key_list_p ("key-list?");
  Variable key_signature_interface_alteration_positions ("key-signature-interface::alteration-positions");
  Variable layout_extract_page_properties ("layout-extract-page-properties");
  Variable lilypond_main ("lilypond-main");
  Variable line_markup ("line-markup");
  Variable f_location ("%location");
  Variable lookup_font ("lookup-font");
  Variable lookup_markup_command ("lookup-markup-command");
  Variable lookup_markup_list_command ("lookup-markup-list-command");
  Variable ly_context_find ("ly:context-find");
  Variable ly_context_set_property_x ("ly:context-set-property!");
  Variable ly_event_p ("ly:event?");
  Variable ly_make_event_class ("ly:make-event-class");
  Variable ly_music_p ("ly:music?");
  Variable make_music ("make-music");
  Variable make_safe_lilypond_module ("make-safe-lilypond-module");
  Variable make_span_event ("make-span-event");
  Variable markup_p ("markup?");
  Variable markup_list_p ("markup-list?");
  Variable midi_program ("midi-program");
#if !GUILEV2
  Variable module_export_all_x ("module-export-all!");
#endif
  Variable f_parser ("%parser");
  Variable percussion_p ("percussion?");
  Variable pure_chain_offset_callback ("pure-chain-offset-callback");
  Variable remove_stencil_warnings ("remove-stencil-warnings");
  Variable scale_layout ("scale-layout");
  Variable scm_to_string ("scm->string");
  Variable score_lines_markup_list ("score-lines-markup-list");
  Variable score_markup ("score-markup");
  Variable scorify_music ("scorify-music");
  Variable span_bar_notify_grobs_of_my_existence ("span-bar::notify-grobs-of-my-existence");
  Variable stencil_whiteout ("stencil-whiteout");
  Variable symbol_list_p ("symbol-list?");
  Variable tremolo_get_music_list ("tremolo::get-music-list");
  Variable type_name ("type-name");
  Variable volta_bracket_calc_hook_visibility ("volta-bracket::calc-hook-visibility");
  Variable write_performances_midis ("write-performances-midis");
}

namespace Syntax {
  Scm_module module ("scm ly-syntax-constructors");

  Variable add_lyrics ("add-lyrics");
  Variable argument_error ("argument-error");
  Variable composed_markup_list ("composed-markup-list");
  Variable context_change ("context-change");
  Variable context_specification ("context-specification");
  Variable event_chord ("event-chord");
  Variable lyric_combine ("lyric-combine");
  Variable lyric_event ("lyric-event");
  Variable multi_measure_rest ("multi-measure-rest");
  Variable music_function ("music-function");
  Variable music_function_call_error ("music-function-call-error");
  Variable partial_markup ("partial-markup");
  Variable partial_music_function ("partial-music-function");
  Variable property_override ("property-override");
  Variable property_override_function ("property-override-function");
  Variable property_revert ("property-revert");
  Variable property_set ("property-set");
  Variable property_set_function ("property-set-function");
  Variable property_unset ("property-unset");
  Variable repeat ("repeat");
  Variable repetition_chord ("repetition-chord");
  Variable sequential_music ("sequential-music");
  Variable simultaneous_music ("simultaneous-music");
  Variable tempo ("tempo");
  Variable unrelativable_music ("unrelativable-music");
  Variable void_music ("void-music");
}
