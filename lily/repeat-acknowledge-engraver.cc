/*   
  repeat-acknowledge-engraver.cc --  implement Repeat_acknowledge_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "engraver.hh"
#include "translator-group.hh"
#include "repeated-music.hh"


/*
  Objective:

  -- set and reset repeatCommands, so Unfolded_repeat_iterator knows
    where to set variables.

  -- collect information passed by Unfolded_repeat_iterator for
   Bar_engraver: writes whichBar property. (TODO: check for
   interactions with timing engraver.)
  
 */
class Repeat_acknowledge_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Repeat_acknowledge_engraver();
  
  virtual void do_post_move_processing ();
  virtual void do_process_music ();
  virtual void do_creation_processing ();
};

void
Repeat_acknowledge_engraver::do_creation_processing ()
{
  daddy_trans_l_->set_property ("repeatCommands", SCM_EOL);
}


Repeat_acknowledge_engraver::Repeat_acknowledge_engraver()
{
}

void
Repeat_acknowledge_engraver::do_post_move_processing ()
{
  Translator_group * tr = daddy_trans_l_->where_defined (ly_symbol2scm ("repeatCommands"));
  if (!tr)
    tr = daddy_trans_l_;

  tr->set_property ("repeatCommands", SCM_EOL);
}

void
Repeat_acknowledge_engraver::do_process_music ()
{
  SCM cs = get_property ("repeatCommands");

  String s = "";
  bool start  = false;
  bool end = false;
  while (gh_pair_p (cs))
    {
      SCM command = gh_car (cs);
      if (command == ly_symbol2scm ("start-repeat"))
	start = true;
      else if (command == ly_symbol2scm ("end-repeat"))
	end = true;
      cs = gh_cdr (cs);      
    }

  if ( start && end )
    s = ":|:";
  else if (start)
    s = "|:";
  else if (end)
    s = ":|";

  if (s != "")
    {
      daddy_trans_l_->set_property ("whichBar", ly_str02scm(s.ch_C()));
    }
}


ADD_THIS_TRANSLATOR(Repeat_acknowledge_engraver);
