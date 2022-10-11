/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2015--2022 by David Kastrup <dak@gnu.org>

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

namespace Guile_user
{
Scm_module module ("guile-user");

Variable apply ("apply");
#if SCM_MAJOR_VERSION == 2
Variable p_auto_compilation_options ("%auto-compilation-options");
#endif
Variable debug_options ("debug-options");
Variable equal ("=");
Variable less ("<");
Variable plus ("+");
Variable make_module ("make-module");
Variable module_export_all_x ("module-export-all!");
Variable module_export_x ("module-export!");
Variable module_public_interface ("module-public-interface");
Variable module_use_x ("module-use!");
Variable symbol_p ("symbol?");
Variable the_root_module ("the-root-module");
} // namespace Guile_user

namespace Compile
{
Scm_module module ("system base compile");

Variable compile ("compile");
#if SCM_MAJOR_VERSION >= 3
Variable default_optimization_level ("default-optimization-level");
#endif
} // namespace Compile

#if SCM_MAJOR_VERSION == 2
namespace Tree_il_optimize
{
Scm_module module ("language tree-il optimize");

Variable
  tree_il_default_optimization_options ("tree-il-default-optimization-options");
} // namespace Tree_il_optimize

namespace Cps_optimize
{
Scm_module module ("language cps optimize");

Variable cps_default_optimization_options ("cps-default-optimization-options");
} // namespace Cps_optimize
#endif

namespace Display
{
Scm_module module ("lily display-lily");

Variable value_to_lily_string ("value->lily-string");
} // namespace Display

namespace Lily
{
Scm_module module ("lily");

Variable all_music_font_encodings ("all-music-font-encodings");
Variable alterations_in_key ("alterations-in-key");
Variable bar_glyph_alist ("bar-glyph-alist");
Variable bar_line_calc_glyph_name_for_direction (
  "bar-line::calc-glyph-name-for-direction");
Variable base_length ("base-length");
Variable beam_exceptions ("beam-exceptions");
Variable beat_structure ("beat-structure");
Variable calc_repeat_slash_count ("calc-repeat-slash-count");
Variable car_less ("car<");
Variable clipped_systems_stencils ("clipped-systems-stencils");
Variable construct_chord_elements ("construct-chord-elements");
Variable default_time_signature_settings ("default-time-signature-settings");
Variable define_markup_command_internal ("define-markup-command-internal");
Variable generate_crop_stencil ("generate-crop-stencil");
Variable generate_preview_stencil ("generate-preview-stencil");
Variable generate_system_stencils ("generate-system-stencils");
Variable grob_compose_function ("grob::compose-function");
Variable grob_offset_function ("grob::offset-function");
Variable hash_table_to_alist ("hash-table->alist");
Variable headers_property_alist_chain ("headers-property-alist-chain");
Variable interpret_markup_list ("interpret-markup-list");
Variable invalidate_alterations ("invalidate-alterations");
Variable key_p ("key?");
Variable key_list_p ("key-list?");
Variable key_signature_interface_alteration_positions (
  "key-signature-interface::alteration-positions");
Variable layout_extract_page_properties ("layout-extract-page-properties");
Variable parse_and_check_version ("parse-and-check-version");
Variable lilypond_main ("lilypond-main");
Variable line_markup ("line-markup");
Variable f_location ("%location");
Variable lookup_font ("lookup-font");
Variable lookup_markup_command ("lookup-markup-command");
Variable lookup_markup_list_command ("lookup-markup-list-command");
Variable ly_context_set_property_x ("ly:context-set-property!");
Variable ly_event_p ("ly:event?");
Variable ly_make_event_class ("ly:make-event-class");
Variable ly_music_p ("ly:music?");
Variable make_concat_markup ("make-concat-markup");
Variable make_music ("make-music");
Variable make_span_event ("make-span-event");
Variable make_tied_lyric_markup ("make-tied-lyric-markup");
Variable markup_p ("markup?");
Variable markup_command_signature ("markup-command-signature");
Variable markup_function_p ("markup-function?");
Variable markup_list_function_p ("markup-list-function?");
Variable markup_list_p ("markup-list?");
Variable markup_to_string ("markup->string");
Variable midi_program ("midi-program");
Variable f_parser ("%parser");
Variable output_scopes ("output-scopes");
Variable percussion_p ("percussion?");
Variable pitchnames ("pitchnames");
Variable pure_chain_offset_callback ("pure-chain-offset-callback");
Variable scale_p ("scale?");
Variable scale_to_factor ("scale->factor");
Variable scale_layout ("scale-layout");
Variable scm_to_string ("scm->string");
Variable score_lines_markup_list ("score-lines-markup-list");
Variable score_markup ("score-markup");
Variable scorify_music ("scorify-music");
Variable stencil_whiteout ("stencil-whiteout");
Variable symbol_list_p ("symbol-list?");
Variable type_name ("type-name");
Variable
  unbroken_or_first_broken_spanner_p ("unbroken-or-first-broken-spanner?");
Variable unbroken_or_last_broken_spanner_p ("unbroken-or-last-broken-spanner?");
Variable
  volta_bracket_calc_hook_visibility ("volta-bracket::calc-hook-visibility");
Variable write_performances_midis ("write-performances-midis");
Variable write_lilypond_book_aux_files ("write-lilypond-book-aux-files");
} // namespace Lily

namespace Loader
{
Scm_module module ("system vm loader");

Variable load_thunk_from_memory ("load-thunk-from-memory");
} // namespace Loader

namespace Page
{
Scm_module module ("lily page");

Variable calc_printable_height ("calc-printable-height");
Variable make_page ("make-page");
Variable page_stencil ("page-stencil");
} // namespace Page

namespace Srfi_1
{
Scm_module module ("srfi srfi-1");

Variable append_reverse ("append-reverse");
Variable delete_duplicates ("delete-duplicates");
Variable lset_union ("lset-union");
} // namespace Srfi_1

namespace Syntax
{
Scm_module module ("lily ly-syntax-constructors");

Variable add_lyrics ("add-lyrics");
Variable argument_error ("argument-error");
Variable composed_markup_list ("composed-markup-list");
Variable context_change ("context-change");
Variable context_create ("context-create");
Variable context_find_or_create ("context-find-or-create");
Variable create_script ("create-script");
Variable create_script_function ("create-script-function");
Variable event_chord ("event-chord");
Variable lyric_combine ("lyric-combine");
Variable lyric_event ("lyric-event");
Variable multi_measure_rest ("multi-measure-rest");
Variable music_function ("music-function");
Variable music_function_call_error ("music-function-call-error");
Variable partial_markup ("partial-markup");
Variable partial_music_function ("partial-music-function");
Variable partial_text_script ("partial-text-script");
Variable property_override ("property-override");
Variable property_revert ("property-revert");
Variable property_set ("property-set");
Variable property_unset ("property-unset");
Variable repeat ("repeat");
Variable repeat_alt ("repeat-alt");
Variable repetition_chord ("repetition-chord");
Variable sequential_alternative_music ("sequential-alternative-music");
Variable sequential_music ("sequential-music");
Variable simultaneous_music ("simultaneous-music");
Variable tempo ("tempo");
Variable unrelativable_music ("unrelativable-music");
Variable void_music ("void-music");
} // namespace Syntax
