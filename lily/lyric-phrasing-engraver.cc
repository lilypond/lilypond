/*   
  new-phrasing-engraver.cc --  implement New_phrasing_engraver

source file of the GNU LilyPond music typesetter

(c) 2003--2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

 */


#include "translator-group.hh"
#include "engraver.hh"
#include "note-head.hh"
#include "lyric-extender.hh"
#include "item.hh"

struct Phrasing_association
{
  String name_;
  Link_array<Grob> lyrics_;
  Link_array<Grob> heads_;
  Link_array<Spanner> past_extenders_;
  Link_array<Spanner> new_extenders_;
  
  bool melisma_;
  
  Phrasing_association()
  {
    melisma_ = false;
  }
  
};

class Lyric_phrasing_engraver : public Engraver
{
public:
  ~Lyric_phrasing_engraver ();
  TRANSLATOR_DECLARATIONS(Lyric_phrasing_engraver);
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();

private:
  void add_lyric_phrasing (Grob_info);
  void add_voice_phrasing (Grob_info);
  void add_lyric_extender (Grob_info);
  Phrasing_association *get_phrasing_assoc (String nm);
  String get_voice_name_for_lyric (Translator_group*tr);
  Link_array<Phrasing_association> assocs_;
};

Lyric_phrasing_engraver::Lyric_phrasing_engraver()
{
}

void
Lyric_phrasing_engraver::acknowledge_grob (Grob_info i)
{
  Grob *h = i.grob_;

  if (Note_head::has_interface (h))
    add_voice_phrasing (i);
  else if (h->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    add_lyric_phrasing (i);
  else if (Lyric_extender::has_interface (h))
    add_lyric_extender (i);
}

Phrasing_association *
Lyric_phrasing_engraver::get_phrasing_assoc (String nm)
{
  Phrasing_association * a=0;
  for (int i=0 ; !a && i < assocs_.size (); i++)
    {
      if (assocs_[i]->name_ == nm)
	a = assocs_[i];
    }

  if (!a)
    {
      a = new Phrasing_association ;
      a->name_ = nm;
      assocs_.push (a);
    }
  return a;
}



String
Lyric_phrasing_engraver::get_voice_name_for_lyric (Translator_group*tr)
{
  SCM voice = tr->get_property ("associatedVoice");
  String nm = tr->id_string_;
  if (gh_string_p (voice))
    nm = ly_scm2string (voice);
  else
    {
      int idx = nm.index_last ('-');
      if (idx >= 0)
	nm = nm.left_string (idx);
    }

  return nm;
}


void
Lyric_phrasing_engraver::add_lyric_extender (Grob_info inf)
{
  Translator_group * tr = inf.origin_trans_->daddy_trans_;
  while (tr && !tr->is_alias (ly_symbol2scm ("LyricsVoice")))
    tr = tr->daddy_trans_;

  if (!tr)
    return;

  
  Phrasing_association *a =  get_phrasing_assoc (get_voice_name_for_lyric (tr));
  a->new_extenders_.push (dynamic_cast<Spanner*> (inf.grob_));  
}


void
Lyric_phrasing_engraver::add_voice_phrasing (Grob_info inf)
{
  Translator_group * tr = inf.origin_trans_->daddy_trans_;
  while (tr && !tr->is_alias (ly_symbol2scm ("Voice")))
    tr = tr->daddy_trans_;

  if (!tr)
    return;

  Phrasing_association *a =  get_phrasing_assoc (tr->id_string_);
  a->heads_.push (inf.grob_);
  a->melisma_ = melisma_busy (inf.origin_trans_);
}

void
Lyric_phrasing_engraver::add_lyric_phrasing (Grob_info inf)
{
  Translator_group * tr = inf.origin_trans_->daddy_trans_;
  while (tr && !tr->is_alias (ly_symbol2scm ("LyricsVoice")))
    tr = tr->daddy_trans_;

  if (!tr)
    return;


  Phrasing_association *a =  get_phrasing_assoc (get_voice_name_for_lyric (tr));
  a->lyrics_.push (inf.grob_);
  a->past_extenders_.clear ();
}

void
Lyric_phrasing_engraver::stop_translation_timestep ()
{
  for (int i = assocs_.size ();  i--; )
    {
      assocs_[i]->heads_.clear ();
      assocs_[i]->lyrics_.clear ();
      assocs_[i]->past_extenders_.concat (assocs_[i]->new_extenders_) ;
      assocs_[i]->new_extenders_.clear ();
    }
}

void
Lyric_phrasing_engraver::process_acknowledged_grobs ()
{
  for (int i = 0; i < assocs_.size ();  i++)
    {
      Phrasing_association * a = assocs_[i];
      if (! (a->heads_.size()  && (a->lyrics_.size () || a->past_extenders_.size ())))
	continue;

      Grob *h = a->heads_[0];	
      Direction alignment = CENTER;
      if (a->melisma_)
	alignment = LEFT;
      
      for (int j = 0; j < a->lyrics_.size (); j++)
	{
	  Grob *l = a->lyrics_[j];
	  if (!l->get_parent (X_AXIS))
	    {
	      l->set_parent (h, X_AXIS);
	      if (alignment)
		l->set_grob_property ("self-alignment-X", gh_int2scm (alignment));
	    }
	}

      for (int j = a->past_extenders_.size(); j--;)
	a->past_extenders_[j]->set_bound (RIGHT, dynamic_cast<Item*> (h));
    }
}

Lyric_phrasing_engraver::~Lyric_phrasing_engraver ()
{
  for (int i =assocs_.size(); i--;)
    delete assocs_[i];
}

ENTER_DESCRIPTION(Lyric_phrasing_engraver,
		  "This engraver combines note heads and lyrics for alignment. ",
		  "",
		  "",
		  "lyric-syllable-interface note-head-interface lyric-extender-interface",
		  "automaticPhrasing associatedVoice",
		  "");

