#include "engraver.hh"
#include "text-item.hh"
#include "request.hh"
#include "item.hh"

class Figured_bass_engraver : public Engraver
{
  
  TRANSLATOR_DECLARATIONS(Figured_bass_engraver);
protected:
  Link_array<Music> figures_;
  Music * rest_req_;

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
      figure_ = 0;
    }

  figures_.clear ();
  rest_req_ = 0;
}

bool
Figured_bass_engraver::try_music (Music*m)
{
  if (m->is_mus_type ("bass-figure-event"))
    {
      figures_.push (m);
      return true;
    }
  else if (m->is_mus_type ("rest-event"))
    {
      rest_req_ = m;
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
      announce_grob(figure_, rest_req_->self_scm()); // todo
      figure_->set_grob_property ("text" , scm_makfrom0str ("-"));
    }
  else if (figures_.size ())
    {
      figure_ = new Item (get_property ("BassFigure"));
      SCM l = SCM_EOL;

      for (int i = 0; i <figures_.size (); i++)
	l = gh_cons (figures_[i]->self_scm(), l);
      figure_->set_grob_property ("causes", l);
      
      announce_grob(figure_, figures_[0]->self_scm()); // todo
    }
}

  
ENTER_DESCRIPTION(Figured_bass_engraver,
/* descr */       "Make figured bass numbers.",
/* creats*/       "BassFigure",
/* accepts */     "rest-event bass-figure-event",
/* acks  */      "",
/* reads */       "",
/* write */       "");
