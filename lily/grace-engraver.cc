/* 
  grace-engraver.cc --  implement Grace_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/

#include "engraver.hh"
#include "context.hh"
#include "warn.hh"

class Grace_engraver : public Engraver
{
protected:
  virtual void start_translation_timestep ();
  virtual void derived_mark ();
  
  TRANSLATOR_DECLARATIONS (Grace_engraver);
  Moment last_moment_;
  SCM grace_settings_;
public:
};


Grace_engraver::Grace_engraver()
{
  grace_settings_ = SCM_EOL;
}

void
Grace_engraver::derived_mark ()
{
  scm_gc_mark (grace_settings_);
}

void
Grace_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();
  if (last_moment_.grace_part_ && !now.grace_part_)
    {
      for (SCM s = grace_settings_; scm_is_pair (s); s = scm_cdr (s))
	{
	  SCM context = scm_caar (s);
	  SCM entry = scm_cdar (s);
	  SCM grob = scm_cadr (entry);
	  SCM sym = scm_caddr (entry);

	  execute_pushpop_property (unsmob_context (context),
				    grob, sym, SCM_UNDEFINED);
	}

      grace_settings_ = SCM_EOL;
    }
  else if (!last_moment_.grace_part_ && now.grace_part_)
    {
      SCM settings = get_property ("graceSettings");

      grace_settings_ = SCM_EOL;
      for (SCM s = settings; scm_is_pair (s); s = scm_cdr (s))
	{
	  SCM entry = scm_car (s);
	  SCM context_name = scm_car (entry);
	  SCM grob = scm_cadr (entry);
	  SCM sym = scm_caddr (entry);
	  SCM val = scm_cadddr (entry);

	  Context *c = context ();
	  while (c  
		 && c->context_name_symbol () != context_name)
	    {
	      c = c->get_parent_context ();	      
	    }

	  if (c) 
	    {
	      execute_pushpop_property (c,
					grob, sym, val);
	      grace_settings_
		= scm_cons (scm_cons (c->self_scm(), entry),  grace_settings_);
	    }
	  else
	    {
	      programming_error ("Cannot find context");
	      scm_display (context_name, scm_current_error_port());
	    }
	}
    }

  last_moment_ = now;
}


ENTER_DESCRIPTION (Grace_engraver,
		   /* descr */       "Set font size and other properties for grace notes.",
		   /* creats*/       "",
		   /* accepts */     "",
		   /* acks  */      "",
		   /* reads */       "graceSettings",
		   /* write */       "");
