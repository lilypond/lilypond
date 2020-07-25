/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2015--2020 Daniel Eble <dan@faithful.be>

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

#ifndef CHANGE_SEQUENCE_ITERATOR_HH
#define CHANGE_SEQUENCE_ITERATOR_HH

#include "music-wrapper-iterator.hh"
#include "context.hh"

/** base for iterators that perform a sequence of timed context changes */
class Change_sequence_iterator : public Music_wrapper_iterator
{
public:
  Change_sequence_iterator () = default;

protected:
  void create_children () override;
  void process (Moment) override;

private:
  // implement in derived class to effect a context change
  virtual void change_to (const std::string &id) = 0;

private:
  // There is no need to protect this in derived_mark() because it is protected
  // via Music_iterator::music_.
  SCM change_list_ = SCM_EOL;
};

#endif /* CHANGE_SEQUENCE_ITERATOR_HH */
