/*
  
  all-font-metrics.cc --  implement All_font_metrics
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "all-font-metrics.hh"

#include "config.hh"
#include "main.hh"
#include "warn.hh"
#include "afm.hh"
#include "tfm.hh"
#include "scm-hash.hh"
#include "kpath.hh"

static const char *default_font_str0_ = "cmr10";

All_font_metrics::All_font_metrics (String path)
{
  afm_p_dict_ = new Scheme_hash_table;
  tfm_p_dict_ = new Scheme_hash_table;
  
  search_path_.parse_path (path);
}

All_font_metrics::~All_font_metrics ()
{
  scm_gc_unprotect_object (afm_p_dict_->self_scm ());
  scm_gc_unprotect_object (tfm_p_dict_->self_scm ());
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
  if (!afm_p_dict_->try_retrieve (sname, &val))
    {
      String file_name;

      if (file_name.is_empty ())
	file_name = search_path_.find (name  + ".afm");

      if (file_name.is_empty ())
	{
	  String p = kpathsea_find_afm (name.to_str0 ());
	  if (p.length ())
	    file_name = p;
	}

      if (file_name.is_empty ())
	return 0;
      
      if (verbose_global_b)
	progress_indication ("[" + file_name);
      val = read_afm_file (file_name);
      unsmob_metrics (val)->file_name_ = file_name;
      
      unsmob_metrics (val)->description_ = scm_cons (name_string, 
						     scm_make_real (1.0));

      if (verbose_global_b)
	progress_indication ("]");

      afm_p_dict_->set (sname, val);
      scm_gc_unprotect_object (val);

      Adobe_font_metric *afm
	= dynamic_cast<Adobe_font_metric*> (unsmob_metrics (val));

      /* Only check checksums if there is one.  We take the risk that
	 some file has valid checksum 0 */
      if (afm->checksum_)
	{
	  Tex_font_metric * tfm = find_tfm (name);
	  
	  /* FIXME: better warning message
	     (maybe check upon startup for feta16.afm, feta16.tfm?) */
	  if (tfm && tfm->info_.checksum != afm->checksum_)
	    {
	      // FIXME: broken sentence
	      String s = _f ("checksum mismatch for font file: `%s'",
			     file_name.to_str0 ());
	      s += " " + _f ("does not match: `%s'",
			     tfm->file_name_.to_str0 ());
	      s += "\n";
	      s += " TFM: " + to_string ((int) tfm->info_.checksum);
	      s += " AFM: " + to_string ((int) afm->checksum_);
	      s += "\n";
	      s += _ ("Rebuild all .afm files, and remove all .pk and .tfm files.");
	      s += "\n";
	      s += _ ("Rerun with -V to show font paths.");
	      s += "\n";
	      s += _("A script for removing font-files is delivered with the source-code:");
	      s += "\n";
	      s += "buildscripts/clean-fonts.sh";
	      error (s);
	    }
	}
    }
  
  return dynamic_cast<Adobe_font_metric*> (unsmob_metrics (val));
}


Tex_font_metric*
All_font_metrics::find_tfm (String name)
{
  SCM sname = ly_symbol2scm (name.to_str0 ());
  SCM name_string = scm_makfrom0str (name.to_str0 ());
  SCM val;
  if (!tfm_p_dict_->try_retrieve (sname, &val))
    {
      String file_name;
      
      if (file_name.is_empty ())
	{
	  String p = kpathsea_find_tfm (name.to_str0 ());
	  if (p.length ())
	    file_name = p;
	}

      if (file_name.is_empty ())
	file_name = search_path_.find (name  + ".tfm");
      if (file_name.is_empty ())
	return 0;

      if (verbose_global_b)
	progress_indication ("[" + file_name);
      
      val = Tex_font_metric::make_tfm (file_name);

      if (verbose_global_b)
	progress_indication ("]");

      unsmob_metrics (val)->file_name_ = file_name;
      unsmob_metrics (val)->description_ = scm_cons (name_string,
						     scm_make_real (1.0));
      tfm_p_dict_->set (sname, val);
      scm_gc_unprotect_object (val);
    }

  return dynamic_cast<Tex_font_metric*> (unsmob_metrics (val));
}

Font_metric*
All_font_metrics::find_font (String name)
{
  if ((name.left_string (4) == "feta") ||
      (name.left_string (8) == "parmesan"))
    {
      Font_metric *f = find_afm (name);
      if (f)
	return f;
      else
      f =find_tfm (name);
      if (f)
	return f ;
    }
  else
    {
      Font_metric * f = find_tfm (name);
      if (f)
	return f;
      else
      f = find_afm (name);
      if (f)
	return f;
    }

  warning (_f ("can't find font: `%s'", name.to_str0 ()));
  warning (_ ("Loading default font"));
  
  String def_name = default_font_str0_;

  /*
    we're in emergency recovery mode here anyway, so don't try to do
    anything smart that runs the risk of failing.  */
  Font_metric*  f= find_afm (def_name);
  if (f)
    return f;

  f =  find_tfm (def_name);
  if (f)
    return f;

  error (_f ("can't find default font: `%s'", def_name.to_str0 ()));
  error (_f ("(search path: `%s')", search_path_.to_string ()));
  error (_ ("Giving up"));

  return 0;
}

All_font_metrics *all_fonts_global;


LY_DEFINE (ly_font_load, "ly:font-load", 1, 0, 0,
	   (SCM name),
	   "Load the font @var{name}. ")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");

  Font_metric * fm = all_fonts_global->find_font (ly_scm2string (name));

  return fm->self_scm ();
}

