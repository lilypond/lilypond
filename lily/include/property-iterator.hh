/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
