/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "musical-request.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "main.hh"
#include "dimensions.hh"
#include "g-text-item.hh"


#include "engraver.hh"
#include "array.hh"
#include "lily-proto.hh"

class Lyric_engraver : public Engraver 
{
protected:
  virtual void do_pre_move_processing();
  virtual bool do_try_music (Music*);
  virtual void do_process_requests();

public:
  Lyric_engraver();
  VIRTUAL_COPY_CONS(Translator);

private:
  Link_array<Lyric_req> lyric_req_l_arr_;
  Link_array<Item> text_p_arr_;
};


ADD_THIS_TRANSLATOR(Lyric_engraver);


Lyric_engraver::Lyric_engraver()
{
}

bool
Lyric_engraver::do_try_music (Music*r)
{
  if (Lyric_req* l = dynamic_cast <Lyric_req *> (r))
    {
      lyric_req_l_arr_.push (l);
      return true;
    }
  return false;
}

void
Lyric_engraver::do_process_requests()
{
  if (text_p_arr_.size ())
    return;

  for (int i=0; i < lyric_req_l_arr_.size (); i++)
    {
      Lyric_req* request_l = lyric_req_l_arr_[i];
      G_text_item* item_p =  new G_text_item;
      item_p->text_str_ = request_l->text_str_;

      Scalar style = get_property ("textstyle", 0);
      if (style.length_i ())
	item_p->style_str_ = style;
      
      text_p_arr_.push (item_p);
      announce_element (Score_element_info (item_p, request_l));
    }
}

void
Lyric_engraver::do_pre_move_processing()
{
  for (int i=0; i < text_p_arr_.size (); i++)
    {
      typeset_element (text_p_arr_[i]);
    }
  text_p_arr_.clear ();
  lyric_req_l_arr_.clear ();
}

