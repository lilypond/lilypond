/*   
  all-font-metrics.cc --  implement All_font_metrics
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "config.h"
#include "main.hh"
#include "all-font-metrics.hh"

#include "warn.hh"
#include "afm.hh"
#include "tfm.hh"
#include "lily-guile.hh"
#include "scm-hash.hh"
#include "kpath.hh"

static const char * default_font_str0_ = "cmr10";

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

Adobe_font_metric *
All_font_metrics::find_afm (String name)
{
  SCM sname = ly_symbol2scm (name.to_str0 ());

  SCM name_string = scm_makfrom0str (name.to_str0 ());

  SCM val;
  
  if (!afm_p_dict_->try_retrieve (sname, &val))
    {
      String path;

      if (path.empty_b ())
	path = search_path_.find (name  + ".afm");

      if (path.empty_b ())
	{
	  String p = ly_find_afm (name.to_str0 ());
	  if (p.length ())
	    path = p;
	}

      if (path.empty_b ())
	return 0;
      
      if (verbose_global_b)
	progress_indication ("[" + path);
      val = read_afm_file (path);
      unsmob_metrics (val)->path_ = path;
      
      unsmob_metrics (val)->description_ = gh_cons (name_string, gh_double2scm (1.0));

      if (verbose_global_b)
	progress_indication ("]");

      afm_p_dict_->set (sname,val);

      scm_gc_unprotect_object (val);


      Adobe_font_metric *afm
	= dynamic_cast<Adobe_font_metric*> (unsmob_metrics (val));

      /*
	only check checksums if there is one.  We take the risk that
	some file has valid checksum 0
      */
      if (afm->checksum_)
	{
	  
	  Tex_font_metric * tfm = find_tfm (name);
	  
	  /* FIXME: better warning message
	     (maybe check upon startup for feta16.afm, feta16.tfm?)
	  */
	  if (tfm && tfm->info_.checksum != afm->checksum_)
	    {
	      String s = _f ("checksum mismatch for font file: `%s'",
			     path.to_str0 ());
	      s += " " + _f ("does not match: `%s'", tfm->path_.to_str0 ()); // FIXME
	      s += "\n";
	      s += " TFM: " + to_string ((int) tfm->info_.checksum);
	      s += " AFM: " + to_string ((int) afm->checksum_);
	      s += "\n";
	      s += _ (" Rebuild all .afm files, and remove all .pk and .tfm files.  Rerun with -V to show font paths.");
	      
	      error (s);
	    }
	}
    }
  
  return dynamic_cast<Adobe_font_metric*> (unsmob_metrics (val));
}


Tex_font_metric *
All_font_metrics::find_tfm (String name)
{
  SCM sname = ly_symbol2scm (name.to_str0 ());
  SCM name_string = scm_makfrom0str (name.to_str0 ());

  SCM val;
  if (!tfm_p_dict_->try_retrieve (sname, &val))
    {
      String path;
      
      if (path.empty_b ())
	{
	  String p = ly_find_tfm (name.to_str0 ());
	  if (p.length ())
	    path = p;
	}

      if (path.empty_b ())
	path = search_path_.find (name  + ".tfm");
      if (path.empty_b ())
	return 0;

      
      if (verbose_global_b)
	progress_indication ("[" + path);
      val = Tex_font_metric::make_tfm (path);

      if (verbose_global_b)
	progress_indication ("]");

      unsmob_metrics (val)->path_ = path;
      unsmob_metrics (val)->description_ = gh_cons (name_string, gh_double2scm (1.0));
      tfm_p_dict_->set (sname, val);

      scm_gc_unprotect_object (val);
    }

  return
    dynamic_cast<Tex_font_metric*> (unsmob_metrics (val));
}


Font_metric *
All_font_metrics::find_font (String name)
{
  Font_metric * f= find_afm (name);
  if (f)
    return f;

  f = find_tfm (name);
  if (f)
    return f;

  warning (_f ("can't find font: `%s'", name.to_str0 ()));
  warning (_ ("Loading default font"));
  
  String def_name = default_font_str0_;

  /*
    we're in emergency recovery mode here anyway, so don't try to do
    anything smart that runs the risk of failing.  */
  f= find_afm (def_name);
  if (f)
    return f;

  f =  find_tfm (def_name);
  if (f)
    return f;

  error (_f ("can't find default font: `%s'", def_name.to_str0 ()));
  error (_f ("(search path: `%s')", search_path_.string ()));
  error (_ ("Giving up"));

  return 0;
}


