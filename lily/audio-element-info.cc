/*
  Audio-element-info.cc -- implement Audio_element_info

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "audio-element-info.hh"

#include "translator-group.hh"
#include "context.hh"

Audio_element_info::Audio_element_info (Audio_element *s, Stream_event *r)
{
  elem_ = s;
  origin_trans_ = 0;
  event_ = r;
}

Audio_element_info::Audio_element_info ()
{
  elem_ = 0;
  event_ = 0;
  origin_trans_ = 0;
}

vector<Context*>
Audio_element_info::origin_contexts (Translator *end) const
{
  Context *t = origin_trans_->context ();
  vector<Context*> r;
  do
    {
      r.push_back (t);
      t = t->get_parent_context ();
    }
  while (t && t != end->context ());

  return r;
}
