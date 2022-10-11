/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef VOLTA_REPEAT_ITERATOR_HH
#define VOLTA_REPEAT_ITERATOR_HH

#include "sequential-iterator.hh"

#include "repeat-styler.hh"

#include <memory>

class Volta_repeat_iterator final : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Volta_repeat_iterator () = default;

  std::shared_ptr<Repeat_styler> get_repeat_styler () { return repeat_styler_; }

protected:
  void create_children () override;
  void process (Moment) override;

private:
  bool empty () const;

private:
  bool started_ = false;
  bool stopped_ = false;
  std::shared_ptr<Repeat_styler> repeat_styler_;
};

#endif // VOLTA_REPEAT_ITERATOR_HH
