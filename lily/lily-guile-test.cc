/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2024 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "lily-guile.hh"

#include <type_traits>
#include <utility>

// We don't have a unit-test framework, but we can sanity-check some things at
// compile time without bloating the lilypond binary.

namespace
{

// `is_to_scm_detected_v<ExplicitT, DeducedT>` is true if
// `to_scm<ExplicitT> (DeducedT)` is invocable.
// C++20: Rewrite this as a concept.
template <typename ExplicitT, typename DeducedT, typename = void>
struct is_to_scm_detected : std::false_type
{
};
template <typename ExplicitT, typename DeducedT>
struct is_to_scm_detected<
  ExplicitT, DeducedT,
  std::void_t<decltype (to_scm<ExplicitT> (std::declval<DeducedT> ()))>>
  : std::true_type
{
};
template <typename ExplicitT, typename DeducedT>
inline constexpr bool is_to_scm_detected_v
  = is_to_scm_detected<ExplicitT, DeducedT>::value;

// `is_from_scm_detected_v<T, S>` is true if `from_scm<T> (S)` is invocable.
// C++20: Rewrite this as a concept.
template <typename T, typename S, typename = void>
struct is_from_scm_detected : std::false_type
{
};
template <typename T, typename S>
struct is_from_scm_detected<
  T, S, std::void_t<decltype (from_scm<T> (std::declval<S> ()))>>
  : std::true_type
{
};
template <typename T, typename S>
inline constexpr bool is_from_scm_detected_v
  = is_from_scm_detected<T, S>::value;

// `is_robust_from_scm_detected_v<ExplicitT, S, DeducedT>` is true if
// `from_scm<ExplicitT> (S, DeducedT)` is invocable.
// C++20: Rewrite this as a concept.
template <typename ExplicitT, typename S, typename DeducedT, typename = void>
struct is_robust_from_scm_detected : std::false_type
{
};
template <typename ExplicitT, typename S, typename DeducedT>
struct is_robust_from_scm_detected<
  ExplicitT, S, DeducedT,
  std::void_t<decltype (from_scm<ExplicitT> (
    std::declval<S> (), std::declval<DeducedT> ()))>> : std::true_type
{
};
template <typename ExplicitT, typename S, typename DeducedT>
inline constexpr bool is_robust_from_scm_detected_v
  = is_robust_from_scm_detected<ExplicitT, S, DeducedT>::value;

} // namespace

//======================================================================
// to_scm (x)
//======================================================================

static_assert (std::is_same_v<                             //
               decltype (to_scm (std::declval<SCM &> ())), //
               SCM &                                       //
               >);

static_assert (std::is_same_v<                                   //
               decltype (to_scm (std::declval<const SCM &> ())), //
               const SCM &                                       //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                              //
               decltype (to_scm (std::declval<SCM &&> ())), //
               const SCM &                                  //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                    //
               decltype (to_scm (std::declval<const SCM &&> ())), //
               const SCM &                                        //
               >);

// `to_scm<reference_type> ()` shouldn't exist.  It probably wouldn't cause any
// problem if it instead just ignored the reference, but consistency with
// from_scm makes more sense.
// TODO: Add `!` to the three cases without it and fix the problem.
static_assert (is_to_scm_detected_v<SCM &, SCM &>);
static_assert (is_to_scm_detected_v<const SCM &, const SCM &>);
static_assert (!is_to_scm_detected_v<SCM &&, SCM &&>);
static_assert (is_to_scm_detected_v<const SCM &&, const SCM &&>);

//======================================================================
// from_scm (scm)
//======================================================================

static_assert (std::is_same_v<                                    //
               decltype (from_scm<SCM> (std::declval<SCM &> ())), //
               SCM &                                              //
               >);

static_assert (std::is_same_v<                                          //
               decltype (from_scm<SCM> (std::declval<const SCM &> ())), //
               const SCM &                                              //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                     //
               decltype (from_scm<SCM> (std::declval<SCM &&> ())), //
               const SCM &                                         //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                           //
               decltype (from_scm<SCM> (std::declval<const SCM &&> ())), //
               const SCM &                                               //
               >);

// `from_scm<reference_type> (scm)` shouldn't exist.
// TODO: Negate these tests and fix the problem.
static_assert (is_from_scm_detected_v<SCM &, SCM &>);
static_assert (is_from_scm_detected_v<const SCM &, const SCM &>);
static_assert (is_from_scm_detected_v<SCM &&, SCM &&>);
static_assert (is_from_scm_detected_v<const SCM &&, const SCM &&>);

//======================================================================
// from_scm (scm, fallback): For SCM->SCM passthrough, the fallback will
// never be used, so we aren't picky about the fallback's qualifiers.
//======================================================================

static_assert (std::is_same_v<                               //
               decltype (from_scm (std::declval<SCM &> (),   //
                                   std::declval<SCM &> ())), //
               SCM &                                         //
               >);

static_assert (std::is_same_v<                                   //
               decltype (from_scm (std::declval<const SCM &> (), //
                                   std::declval<SCM &> ())),     //
               const SCM &                                       //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                               //
               decltype (from_scm (std::declval<SCM &&> (),  //
                                   std::declval<SCM &> ())), //
               const SCM &                                   //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                    //
               decltype (from_scm (std::declval<const SCM &&> (), //
                                   std::declval<SCM &> ())),      //
               const SCM &                                        //
               >);

//----------------------------------------------------------------------

static_assert (std::is_same_v<                                     //
               decltype (from_scm (std::declval<SCM &> (),         //
                                   std::declval<const SCM &> ())), //
               SCM &                                               //
               >);

static_assert (std::is_same_v<                                     //
               decltype (from_scm (std::declval<const SCM &> (),   //
                                   std::declval<const SCM &> ())), //
               const SCM &                                         //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                     //
               decltype (from_scm (std::declval<SCM &&> (),        //
                                   std::declval<const SCM &> ())), //
               const SCM &                                         //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                     //
               decltype (from_scm (std::declval<const SCM &&> (),  //
                                   std::declval<const SCM &> ())), //
               const SCM &                                         //
               >);

//----------------------------------------------------------------------

static_assert (std::is_same_v<                                //
               decltype (from_scm (std::declval<SCM &> (),    //
                                   std::declval<SCM &&> ())), //
               SCM &                                          //
               >);

static_assert (std::is_same_v<                                   //
               decltype (from_scm (std::declval<const SCM &> (), //
                                   std::declval<SCM &&> ())),    //
               const SCM &                                       //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                //
               decltype (from_scm (std::declval<SCM &&> (),   //
                                   std::declval<SCM &&> ())), //
               const SCM &                                    //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                    //
               decltype (from_scm (std::declval<const SCM &&> (), //
                                   std::declval<SCM &&> ())),     //
               const SCM &                                        //
               >);

//----------------------------------------------------------------------

static_assert (std::is_same_v<                                      //
               decltype (from_scm (std::declval<SCM &> (),          //
                                   std::declval<const SCM &&> ())), //
               SCM &                                                //
               >);

static_assert (std::is_same_v<                                      //
               decltype (from_scm (std::declval<const SCM &> (),    //
                                   std::declval<const SCM &&> ())), //
               const SCM &                                          //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                      //
               decltype (from_scm (std::declval<SCM &&> (),         //
                                   std::declval<const SCM &&> ())), //
               const SCM &                                          //
               >);

// TODO: This should not convert a `&&` to a `&`.  It should return `SCM`.
static_assert (std::is_same_v<                                      //
               decltype (from_scm (std::declval<const SCM &&> (),   //
                                   std::declval<const SCM &&> ())), //
               const SCM &                                          //
               >);

// `from_scm<reference_type> (scm, fallback)` shouldn't exist.
// TODO: Negate these tests and fix the problem.
static_assert (is_robust_from_scm_detected_v<SCM &, SCM &, SCM &>);
static_assert (
  is_robust_from_scm_detected_v<const SCM &, const SCM &, const SCM &>);
static_assert (is_robust_from_scm_detected_v<SCM &&, SCM &&, SCM &&>);
static_assert (
  is_robust_from_scm_detected_v<const SCM &&, const SCM &&, const SCM &&>);
