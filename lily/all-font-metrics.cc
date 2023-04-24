/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "all-font-metrics.hh"

#include "file-path.hh"
#include "font-config.hh"
#include "string-convert.hh"
#include "international.hh"
#include "open-type-font.hh"
#include "pango-font.hh"
#include "scm-hash.hh"
#include "warn.hh"

#include <string>

#include <fontconfig/fontconfig.h>
#include <pango/pangofc-fontmap.h>

unique_fcconfig_ptr All_font_metrics::emmentaler_font_config_ = nullptr;
PangoFT2FontMap *All_font_metrics::emmentaler_pango_ft2_fontmap_ = nullptr;

Index_to_charcode_map const *
All_font_metrics::get_index_to_charcode_map (const std::string &filename,
                                             int face_index, FT_Face face)
{
  std::string key = filename + std::to_string (face_index);
  if (filename_charcode_maps_map_.find (key)
      == filename_charcode_maps_map_.end ())
    filename_charcode_maps_map_[key] = make_index_to_charcode_map (face);

  return &filename_charcode_maps_map_[key];
}

static void
substitute_with_lily_config (FcPattern *pat, void *config)
{
  FcConfig *fcconfig = static_cast<FcConfig *> (config);
  FcConfigSubstitute (fcconfig, pat, FcMatchPattern);
}

All_font_metrics::All_font_metrics (File_path search_path,
                                    All_font_metrics *previous)
  : search_path_ (search_path)
{
  pango_dict_ = nullptr;
  otf_dict_ = nullptr;

  smobify_self ();

  otf_dict_ = unsmob<Scheme_hash_table> (Scheme_hash_table::make_smob ());
  pango_dict_ = unsmob<Scheme_hash_table> (Scheme_hash_table::make_smob ());

  // We accept a previous All_font_metrics object (from the last session, i.e.,
  // .ly file processed) in order to avoid recreating an FcConfig if no app
  // fonts were added (the common case), as that takes a bit of time.  The
  // previous object won't be used further, so we can take ownership of its
  // FcConfig if we want.
  if (previous && !previous->font_config_has_app_fonts_)
    font_config_ = std::move (previous->font_config_);
  else
    font_config_ = make_font_config (/* emmentaler */ false);

  // Unlike the general FcConfig, the Emmentaler FcConfig never needs resetting
  // since it is never mutated.
  if (!emmentaler_font_config_) // first initialization
    emmentaler_font_config_ = make_font_config (/* emmentaler */ true);

  pango_dpi_ = PANGO_RESOLUTION;

  auto make_font_map = [&] (FcConfig *conf) {
    PangoFontMap *pfm = pango_ft2_font_map_new ();
    PangoFT2FontMap *res = PANGO_FT2_FONT_MAP (pfm);

    pango_ft2_font_map_set_resolution (res, pango_dpi_, pango_dpi_);
    PangoFcFontMap *fcm = PANGO_FC_FONT_MAP (res);
    assert (fcm);
    pango_fc_font_map_set_config (fcm, conf);

    // Before searching a font pattern with Fontconfig, FcConfigSubstitute should
    // be called on the pattern with the appropriate FcConfig so that the
    // configuration can define transformations to be applied on the pattern.
    // LilyPond does this to define font aliases.  Unfortunately, Pango has a bug
    // with this when using custom FcConfigs: it calls FcConfigSubstitute with the
    // global, default FcConfig instead of the font map's FcConfig.  This is
    // https://gitlab.gnome.org/GNOME/pango/-/issues/743 .  Work around it by
    // adding an extra function to transform the pattern, calling
    // FcConfigSubstitute with the right FcConfig.
    // pango_ft2_font_map_set_default_substitute is deprecated as of Pango 1.46 in
    // favor of pango_fc_font_map_set_default_substitute, but we still support
    // 1.44.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    pango_ft2_font_map_set_default_substitute (res, substitute_with_lily_config,
                                               font_config_.get (), nullptr);
#pragma GCC diagnostic pop
    return res;
  };

  pango_ft2_fontmap_ = make_font_map (font_config_.get ());
  if (!emmentaler_pango_ft2_fontmap_) // first initialization
    {
      emmentaler_pango_ft2_fontmap_
        = make_font_map (emmentaler_font_config_.get ());
    }
}

All_font_metrics::~All_font_metrics ()
{
  g_object_unref (pango_ft2_fontmap_);
}

SCM
All_font_metrics::mark_smob () const
{
  if (pango_dict_)
    scm_gc_mark (pango_dict_->self_scm ());
  if (otf_dict_)
    return otf_dict_->self_scm ();
  return SCM_UNDEFINED;
}

Pango_font *
All_font_metrics::find_pango_font (PangoFontDescription const *description,
                                   bool is_emmentaler, Real output_scale)
{
  gchar *pango_fn = pango_font_description_to_filename (description);
  SCM key = ly_symbol2scm (pango_fn);

  SCM val;
  if (!pango_dict_->try_retrieve (key, &val))
    {
      debug_output ("[" + std::string (pango_fn), true); // start on a new line

      PangoFT2FontMap *map
        = is_emmentaler ? emmentaler_pango_ft2_fontmap_ : pango_ft2_fontmap_;
      Pango_font *pf = new Pango_font (map, description, output_scale);

      val = pf->self_scm ();
      pango_dict_->set (key, val);
      pf->unprotect ();

      debug_output ("]", false);

      pf->description_ = scm_cons (SCM_BOOL_F, to_scm (1.0));
    }
  g_free (pango_fn);
  return unsmob<Pango_font> (val);
}

Open_type_font *
All_font_metrics::find_otf_font (const std::string &name)
{
  SCM sname = ly_symbol2scm (name);
  Open_type_font *ret;
  SCM val;
  if (!otf_dict_->try_retrieve (sname, &val))
    {
      std::string file_name = search_path_.find (name + ".otf");
      if (file_name.empty ())
        {
          error (_f ("cannot find font '%s' (search path: %s)", name.c_str (),
                     search_path_.to_string ().c_str ()));
        }

      debug_output ("[" + file_name, true); // start on a new line
      ret = new Open_type_font (file_name);

      debug_output ("]", false);

      SCM name_string = ly_string2scm (name);
      ret->description_ = scm_cons (name_string, to_scm (1.0));
      otf_dict_->set (sname, ret->self_scm ());
      ret->unprotect ();
    }
  else
    {
      ret = unsmob<Open_type_font> (val);
      assert (ret);
    }
  return ret;
}

void
All_font_metrics::font_config_changed ()
{
  // Pango wants to be informed if the Fontconfig configuration parameters are
  // modified.
  pango_fc_font_map_config_changed (PANGO_FC_FONT_MAP (pango_ft2_fontmap_));
  // Remember that we shouldn't reuse this FcConfig in the next session, as it
  // now contains application fonts that we don't want to bleed over the next
  // .ly file.
  font_config_has_app_fonts_ = true;
}

static std::string
display_fontset (FcFontSet *fs)
{
  std::string retval;

  int j;
  for (j = 0; j < fs->nfont; j++)
    {
      unique_stdlib_ptr<FcChar8> font (FcNameUnparse (fs->fonts[j]));
      FcChar8 *str;
      if (FcPatternGetString (fs->fonts[j], FC_FILE, 0, &str) == FcResultMatch)
        retval += String_convert::form_string ("FILE %s\n", str);
      if (FcPatternGetString (fs->fonts[j], FC_INDEX, 0, &str) == FcResultMatch)
        retval += String_convert::form_string ("INDEX %s\n", str);
      if (FcPatternGetString (fs->fonts[j], FC_FAMILY, 0, &str)
          == FcResultMatch)
        retval += String_convert::form_string ("family %s\n ", str);
      if (FcPatternGetString (fs->fonts[j], "designsize", 0, &str)
          == FcResultMatch)
        retval += String_convert::form_string ("designsize %s\n ", str);

      retval += String_convert::form_string (
        "%s\n", reinterpret_cast<const char *> (font.get ()));
    }

  return retval;
}

static std::string
display_strlist (char const *what, FcStrList *slist)
{
  std::string retval;
  while (FcChar8 *dir = FcStrListNext (slist))
    {
      retval += String_convert::form_string ("%s: %s\n", what, dir);
    }
  return retval;
}

static std::string
display_config (FcConfig *fcc)
{
  std::string retval;
  retval += display_strlist ("Config files", FcConfigGetConfigFiles (fcc));
  retval += display_strlist ("Config dir", FcConfigGetConfigDirs (fcc));
  retval += display_strlist ("Font dir", FcConfigGetFontDirs (fcc));
  return retval;
}

static std::string
display_list (FcConfig *fcc)
{
  FcPattern *pat = FcPatternCreate ();

  FcObjectSet *os = 0;
  if (!os)
    os = FcObjectSetBuild (FC_FAMILY, FC_STYLE, nullptr);

  FcFontSet *fs = FcFontList (fcc, pat, os);
  FcObjectSetDestroy (os);
  if (pat)
    FcPatternDestroy (pat);

  std::string retval;
  if (fs)
    {
      retval = display_fontset (fs);
      FcFontSetDestroy (fs);
    }
  return retval;
}

void
All_font_metrics::display_fonts ()
{
  std::string str = display_list (font_config_.get ());
  str += display_config (font_config_.get ());
  progress_indication (str);
}

std::string
All_font_metrics::get_font_file (const std::string &name)
{
  FcPattern *pat = FcPatternCreate ();
  FcPatternAddString (pat, FC_FAMILY,
                      reinterpret_cast<const FcChar8 *> (name.c_str ()));
  FcConfigSubstitute (font_config_.get (), pat, FcMatchPattern);
  FcDefaultSubstitute (pat);

  FcResult result;
  std::string file_result;

  pat = FcFontMatch (font_config_.get (), pat, &result);

  FcChar8 *str = nullptr;
  if (FcPatternGetString (pat, FC_FILE, 0, &str) == FcResultMatch)
    file_result = reinterpret_cast<char const *> (str);

  FcPatternDestroy (pat);

  return file_result;
}

void
All_font_metrics::add_font_directory (const std::string &name)
{
  if (!FcConfigAppFontAddDir (
        font_config_.get (), reinterpret_cast<const FcChar8 *> (name.c_str ())))
    error (_f ("failed adding font directory: %s", name.c_str ()));
  else
    debug_output (_f ("Adding font directory: %s", name.c_str ()));

  font_config_changed ();
}

void
All_font_metrics::add_font_file (const std::string &name)
{
  if (!FcConfigAppFontAddFile (
        font_config_.get (), reinterpret_cast<const FcChar8 *> (name.c_str ())))
    error (_f ("failed adding font file: %s", name.c_str ()));
  else
    debug_output (_f ("Adding font file: %s", name.c_str ()));

  font_config_changed ();
}
