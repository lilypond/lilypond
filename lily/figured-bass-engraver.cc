#include "engraver.hh"
#include "text-item.hh"
#include "musical-request.hh"
#include "item.hh"

class Figured_bass_engraver : public Engraver
{
  
  TRANSLATOR_DECLARATIONS(Figured_bass_engraver);
protected:
  Link_array<Note_req> figures_;
  Rest_req * rest_req_;

  Grob * figure_;
  
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void process_music ();
};


Figured_bass_engraver::Figured_bass_engraver()
{
  figure_ = 0;
  rest_req_ = 0;
}

void
Figured_bass_engraver::stop_translation_timestep ()
{
  if (figure_)
    {
      typeset_grob (figure_);
      figure_ =00;
    }

  figures_.clear ();
}

bool
Figured_bass_engraver::try_music (Music*m)
{
  if (Note_req* n = dynamic_cast<Note_req*> (m))
    {
      figures_.push (n);
      return true;
    }
  else if (Rest_req * r = dynamic_cast<Rest_req*> (m))
    {
      rest_req_ = r;
      return true;
    }
  return false;
}

void
Figured_bass_engraver::process_music ()
{
  if (rest_req_)
    {
      figure_ = new Item (get_property ("BassFigure"));
      announce_grob (figure_, rest_req_); // todo
      figure_->set_grob_property ("text" , gh_str02scm ("-"));
    }
  else if (figures_.size ())
    {
      figure_ = new Item (get_property ("BassFigure"));
      announce_grob (figure_, figures_[0]); // todo
      SCM flist = SCM_EOL;
      for (int i = 0; i < figures_.size (); i++)
	{
	  Note_req * n = figures_[i];
	  Pitch *p = unsmob_pitch (n->get_mus_property ("pitch"));
	  
	  String fstr = to_str (p->steps ()+ 1);
	  
	  SCM one_fig = ly_str02scm(fstr.ch_C ());

	  if (p->alteration_i_ || to_boolean (n->get_mus_property ("force-accidental") ))
	    {
	      SCM alter = scm_assoc (gh_int2scm (p->alteration_i_),
				     figure_->get_grob_property ("accidental-alist"));
	      if (gh_pair_p (alter))
		{
		  one_fig = scm_list_n (ly_symbol2scm ("columns"),
				     one_fig,
				     ly_cdr(alter),
				     SCM_UNDEFINED);
		}
	    }
	  
	  flist = gh_cons (one_fig, flist);
	}

      flist = gh_cons (ly_symbol2scm ("lines"), flist);

      figure_-> set_grob_property ("text", flist);
    }
}

  
ENTER_DESCRIPTION(Figured_bass_engraver,
/* descr */       "Make volta brackets",
/* creats*/       "BassFigure",
/* acks  */       "",
/* reads */       "",
/* write */       "");
