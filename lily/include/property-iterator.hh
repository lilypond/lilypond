/*
  property-iterator.hh -- declare Property_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PROPERTY_ITERATOR_HH
#define PROPERTY_ITERATOR_HH

#include "simple-music-iterator.hh"

/**
   Iterate a property.
*/
class Property_iterator : public Simple_music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_SCHEME_CALLBACK (once_finalization, (SCM, SCM));
  DECLARE_CLASSNAME(Property_iterator);

protected:
  virtual void do_quit ();
  virtual void process (Moment);
};

/**
   Iterate a property.
*/
class Property_unset_iterator : public Simple_music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_CLASSNAME(Property_unset_iterator);
protected:
  virtual void process (Moment);
};

class Push_property_iterator : public Simple_music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_SCHEME_CALLBACK (once_finalization, (SCM, SCM));
  DECLARE_CLASSNAME(Push_property_iterator);
protected:
  virtual void process (Moment);
  virtual void do_quit ();
};

class Pop_property_iterator : public Simple_music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_CLASSNAME(Pop_property_iterator);
protected:
  virtual void process (Moment);
};

#endif // PROPERTY_ITERATOR_HH
