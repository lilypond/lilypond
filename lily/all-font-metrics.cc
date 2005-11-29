/*
  all-font-metrics.cc -- implement All_font_metrics

  source file of the GNU LilyPond music typesetter

  (c) 1999--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "all-font-metrics.hh"

#include "open-type-font.hh"
#include "main.hh"
#include "warn.hh"
#include "afm.hh"
#include "tfm.hh"
#include "scm-hash.hh"
#include "pango-font.hh"

static char const *default_font_str0_ = "cmr10";

All_font_metrics::All_font_metrics (String path)
{
  afm_dict_ = new Scheme_hash_table;
  tfm_dict_ = new Scheme_hash_table;
  otf_dict_ = new Scheme_hash_table;

#if HAVE_PANGO_FT2
  PangoFontMap *pfm = pango_ft2_font_map_new ();

  pango_ft2_fontmap_
    = G_TYPE_CHECK_INSTANCE_CAST (pfm,
				  PANGO_TYPE_FT2_FONT_MAP,
				  PangoFT2FontMap);
  pango_dpi_ = 1200;
  pango_ft2_font_map_set_resolution (pango_ft2_fontmap_,
				     pango_dpi_, pango_dpi_);

  pango_dict_ = new Scheme_hash_table;
#endif

  search_path_.parse_path (path);
}

All_font_metrics::~All_font_metrics ()
{
  afm_dict_->unprotect ();
  tfm_dict_->unprotect ();
  otf_dict_->unprotect ();

#if HAVE_PANGO_FT2
  pango_dict_->unprotect ();
  g_object_unref (pango_ft2_fontmap_);
#endif
}

All_font_metrics::All_font_metrics (All_font_metrics const &)
{
}

#if HAVE_PANGO_FT2

Pango_font *
All_font_metrics::find_pango_font (PangoFontDescription *description,
				   Real magnification,
				   Real output_scale
				   )
{
  pango_font_description_set_size (description,
				   gint (magnification *
					 pango_font_description_get_size (description)));

  gchar *pango_fn = pango_font_description_to_filename (description);
  SCM key = ly_symbol2scm (pango_fn);

  SCM val;
  if (!pango_dict_->try_retrieve (key, &val))
    {
      if (be_verbose_global)
	progress_indication ("[" + String (pango_fn));

      Pango_font *pf = new Pango_font (pango_ft2_fontmap_,
				       description,
				       output_scale
				       );
      
      val = pf->self_scm ();
      pango_dict_->set (key, val);
      pf->unprotect ();

      if (be_verbose_global)
	progress_indication ("]");

      pf->description_ = scm_cons (SCM_BOOL_F,
				   scm_from_double (1.0));
    }
  g_free (pango_fn);
  return dynamic_cast<Pango_font *> (unsmob_metrics (val));
}

#endif

String
kpathsea_find_file (String name, String ext)
{
  name += "." + ext;
  String path = global_path.find (name);
  if (path.length () > 0)
    return path;

  static SCM proc;
  if (!proc)
    {
      SCM module = scm_c_resolve_module ("scm kpathsea");
      proc = scm_c_module_lookup (module, "ly:kpathsea-find-file");
      proc = scm_variable_ref (proc);
    }

  if (ly_is_procedure (proc))
    {
      SCM kp_result = scm_call_1 (proc, scm_makfrom0str (name.to_str0 ()));
      if (scm_is_string (kp_result))
	return ly_scm2string (kp_result);
    }

  return "";
}

/*
  TODO: our AFM handling is broken: the units in an AFM file are
  relative to the design size (1000 units = 1 designsize). Hence we
  should include design size when generating an AFM metric.

  ugr: copied from find_tfm.
*/
Adobe_font_metric *
All_font_metrics::find_afm (String name)
{
  SCM sname = ly_symbol2scm (name.to_str0 ());
  SCM name_string = scm_makfrom0str (name.to_str0 ());
  SCM val;
  if (!afm_dict_->try_retrieve (sname, &val))
    {
      String file_name;

      if (file_name.is_empty ())
	file_name = search_path_.find (name + ".afm");

      if (file_name.is_empty ())
	{
	  String p = kpathsea_find_file (name, "afm");
	  if (p.length ())
	    file_name = p;
	}

      if (file_name.is_empty ())
	return 0;

      if (be_verbose_global)
	progress_indication ("[" + file_name);
      val = read_afm_file (file_name);
      unsmob_metrics (val)->file_name_ = file_name;

      unsmob_metrics (val)->description_ = scm_cons (name_string,
						     scm_from_double (1.0));

      if (be_verbose_global)
	progress_indication ("]");

      afm_dict_->set (sname, val);
      unsmob_metrics (val)->unprotect ();

      Adobe_font_metric *afm
	= dynamic_cast<Adobe_font_metric *> (unsmob_metrics (val));

      /* Only check checksums if there is one.  We take the risk that
	 some file has valid checksum 0 */
      if (afm->checksum_)
	{
	  Tex_font_metric *tfm = find_tfm (name);

	  /* FIXME: better warning message
	     (maybe check upon startup for feta16.afm, feta16.tfm?) */
	  if (tfm && tfm->info ().checksum != afm->checksum_)
	    {
	      // FIXME: broken sentence
	      String s = _f ("checksum mismatch for font file: `%s'",
			     file_name.to_str0 ());
	      s += " " + _f ("does not match: `%s'",
			     tfm->file_name_.to_str0 ());
	      s += "\n";
	      s += " TFM: " + to_string ((int) tfm->info ().checksum);
	      s += " AFM: " + to_string ((int) afm->checksum_);
	      s += "\n";
	      s += _ ("Rebuild all .afm files, and remove all .pk and .tfm files.");
	      s += "\n";
	      s += _ ("Rerun with -V to show font paths.");
	      s += "\n";
	      s += _ ("A script for removing font-files is delivered with the source-code:");
	      s += "\n";
	      s += "buildscripts/clean-fonts.sh";
	      error (s);
	    }
	}
    }

  return dynamic_cast<Adobe_font_metric *> (unsmob_metrics (val));
}

Open_type_font *
All_font_metrics::find_otf (String name)
{
  SCM sname = ly_symbol2scm (name.to_str0 ());
  SCM name_string = scm_makfrom0str (name.to_str0 ());
  SCM val;
  if (!otf_dict_->try_retrieve (sname, &val))
    {
      String file_name;

      if (file_name.is_empty ())
	file_name = search_path_.find (name + ".otf");
      if (file_name.is_empty ())
	return 0;

      if (be_verbose_global)
	progress_indication ("[" + file_name);

      val = Open_type_font::make_otf (file_name);

      if (be_verbose_global)
	progress_indication ("]");

      unsmob_metrics (val)->file_name_ = file_name;
      unsmob_metrics (val)->description_ = scm_cons (name_string,
						     scm_from_double (1.0));
      otf_dict_->set (sname, val);
      unsmob_metrics (val)->unprotect ();
    }

  return dynamic_cast<Open_type_font *> (unsmob_metrics (val));
}

Tex_font_metric *
All_font_metrics::find_tfm (String name)
{
  SCM sname = ly_symbol2scm (name.to_str0 ());
  SCM name_string = scm_makfrom0str (name.to_str0 ());
  SCM val;
  if (!tfm_dict_->try_retrieve (sname, &val))
    {
      String file_name;

      if (file_name.is_empty ())
	{
	  /* FIXME: should add "cork-" prefix to lm* fonts.  How to do
	     that, cleanly?  */
	  String p = kpathsea_find_file (name, "tfm");
	  if (p.length ())
	    file_name = p;
	}

      if (file_name.is_empty ())
	file_name = search_path_.find (name + ".tfm");
      if (file_name.is_empty ())
	return 0;

      if (be_verbose_global)
	progress_indication ("[" + file_name);

      val = Tex_font_metric::make_tfm (file_name);

      if (be_verbose_global)
	progress_indication ("]");

      unsmob_metrics (val)->file_name_ = file_name;
      unsmob_metrics (val)->description_ = scm_cons (name_string,
						     scm_from_double (1.0));
      tfm_dict_->set (sname, val);
      unsmob_metrics (val)->unprotect ();
    }

  return dynamic_cast<Tex_font_metric *> (unsmob_metrics (val));
}

Font_metric *
All_font_metrics::find_font (String name)
{
  Font_metric *f = find_otf (name);

  if (!f
      && (name.left_string (4) == "feta"
	  || name.left_string (8) == "parmesan"
	  || name.left_string (2) == "lm"))
    {
      f = find_afm (name);
      if (!f)
	f = find_tfm (name);
    }
  else if (!f)
    {
      f = find_tfm (name);
      if (!f)
	f = find_afm (name);
    }

  if (!f)
    {
      warning (_f ("can't find font: `%s'", name.to_str0 ()));
      warning (_ ("loading default font"));
    }

  String def_name = default_font_str0_;

  /* We're in emergency recovery mode here anyway, so don't try to do
     anything smart that runs the risk of failing.  */
  if (!f)
    f = find_afm (def_name);

  if (!f)
    f = find_tfm (def_name);

  if (!f)
    {
      error (_f ("can't find default font: `%s'", def_name.to_str0 ()));
      error (_f ("(search path: `%s')", search_path_.to_string ()));
      error (_ ("giving up"));
    }

  return f;
}

All_font_metrics *all_fonts_global;

LY_DEFINE (ly_font_load, "ly:font-load", 1, 0, 0,
	   (SCM name),
	   "Load the font @var{name}. ")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");

  Font_metric *fm = all_fonts_global->find_font (ly_scm2string (name));

  return fm->self_scm ();
}

