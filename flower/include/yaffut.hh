// Copyright 2006--2022 Rutger E.W. van Beusekom.
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef __YAFFUT_H__
#define __YAFFUT_H__

#include "config.hh"
#if HAVE_CXA_DEMANGLE
#include <cxxabi.h>
#else
#include <typeinfo>
#endif

#include <cmath>
#include <cstring>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <unistd.h>

#define YAFFUT_STRINGIZE(x) YAFFUT_STRINGIZE_ (x)
#define YAFFUT_STRINGIZE_(x) #x

#define __YAFFUT_AT__ __FILE__ ":" YAFFUT_STRINGIZE (__LINE__) ": "
#ifndef __AT__
#define __AT__ __YAFFUT_AT__
#endif

#define YAFFUT_EQUAL(e, a)                                                     \
  yaffut::equal (e, a, __YAFFUT_AT__, "EQUAL(" #e " == " #a ") failed ")
#ifndef EQUAL
#define EQUAL YAFFUT_EQUAL
#endif

#define YAFFUT_UNEQUAL(e, a)                                                   \
  yaffut::unequal (e, a, __YAFFUT_AT__, "UNEQUAL(" #e " != " #a ") failed ")
#ifndef UNEQUAL
#define UNEQUAL YAFFUT_UNEQUAL
#endif

#define YAFFUT_CHECK(e)                                                        \
  yaffut::check (e, __YAFFUT_AT__, "CHECK(" #e ") failed ")
#ifndef CHECK
#define CHECK YAFFUT_CHECK
#endif

#define YAFFUT_FAIL(s) yaffut::fail (s, __YAFFUT_AT__);
#ifndef FAIL
#define FAIL YAFFUT_FAIL
#endif

#define YAFFUT_ASSERT_THROW(s, e)                                              \
  try                                                                          \
    {                                                                          \
      s;                                                                       \
      throw yaffut::failure (__YAFFUT_AT__, #s " failed to throw");            \
    }                                                                          \
  catch (const e &)                                                            \
    {                                                                          \
    }
#ifndef ASSERT_THROW
#define ASSERT_THROW YAFFUT_ASSERT_THROW
#endif

namespace yaffut
{

template <typename T>
std::string
demangle ()
{
#if HAVE_CXA_DEMANGLE
  size_t sz;
  int status;
  char *ptr = abi::__cxa_demangle (typeid (T).name (), 0, &sz, &status);
  std::string name (ptr ? ptr : "", ptr ? strlen (ptr) : 0);
  if (ptr)
    {
      free (ptr);
    }
  std::string::size_type pos = name.rfind ("::");
  if (pos != std::string::npos)
    {
      name = name.substr (pos + 2);
    }
  return name;
#else
  return typeid (T).name ();
#endif
}

struct ITest
{
  virtual ~ITest () {}
};

class Factory
{
public:
  typedef ITest *(*Create_t) ();

private:
  typedef std::map<std::string, Create_t> Tests_t;
  Tests_t m_Tests;
  size_t m_fail;
  size_t m_pass;
  bool m_verbose = true;

private:
  Factory () {}
  ~Factory () {}
  static bool EqualsSuiteName (std::string const &name, std::string const &s)
  {
    return name.find (':') >= name.length () - 2
           && s.substr (0, name.length ()) == name;
  }

public:
  static Factory &Instance ()
  {
    static Factory instance;
    return instance;
  }
  void Register (const std::string &name, Create_t create)
  {
    m_Tests[name] = create;
  }
  size_t Fail () { return m_fail; }
  void List (const std::string &name)
  {
    for (Tests_t::const_iterator it = m_Tests.begin (); it != m_Tests.end ();
         ++it)
      {
        if (name.empty () || it->first == name
            || EqualsSuiteName (name, it->first))
          std::cout << it->first << std::endl;
      }
  }
  void Run (const std::string &name)
  {
    for (Tests_t::const_iterator it = m_Tests.begin (); it != m_Tests.end ();
         ++it)
      {
        if ("All" == name || it->first == name
            || EqualsSuiteName (name, it->first))
          {
            try
              {
                if (m_verbose)
                  {
                    std::cout << std::endl << it->first << ' ' << std::flush;
                  }

                {
                  std::unique_ptr<ITest> test (it->second ());
                }

                if (m_verbose)
                  {
                    std::cout << "[OK]" << std::flush;
                  }

                ++m_pass;
              }
            catch (const std::exception &e)
              {
                if (!m_verbose)
                  {
                    std::cout << std::flush << std::endl << it->first << ' ';
                  }
                std::cout << "[FAIL]\n  " << e.what () << std::flush;
                ++m_fail;
              }
            catch (...)
              {
                if (!m_verbose)
                  {
                    std::cout << std::flush << std::endl << it->first << ' ';
                  }
                std::cout << "[FAIL]\n  unknown exception" << std::flush;
                ++m_fail;
              }
          }
      }
  }
  void Report ()
  {
    const size_t size = m_Tests.size ();
    const size_t total = m_pass + m_fail;
    std::cout << std::endl;
    if (m_verbose || (total < size))
      {
        std::cout << "[TOTAL](" << total << '/' << size << ")" << std::endl;
      }
    if (m_verbose)
      {
        std::cout << "[OK](" << m_pass << '/' << size << ")" << std::endl;
      }
    if (m_fail)
      std::cout << "[FAIL](" << m_fail << '/' << size << ")" << std::endl;
  }
  void SetVerbose (bool verbose = true) { m_verbose = verbose; }
  int Main (int argc, const char *argv[])
  {
    int opti = 1;
    for (/**/; (opti < argc) && (argv[opti][0] == '-'); ++opti)
      {
        const std::string opt (argv[opti]);

        if ((opt == "-h") || (opt == "--help"))
          {
            std::cout << "Yaffut - Yet Another Framework For Unit Testing.\n\n"
                         "Usage: yaffut [OPTION] [Suite:|Suite::Test]...\n\n"
                         "Options:\n"
                         "  -h, --help   show this help\n"
                         "  -l, --list   list test cases\n"
                         "  -q, --quiet  do not mention passing tests\n";
            return 0;
          }
        else if ((opt == "-l") || (opt == "--list"))
          {
            Factory::Instance ().List (argc > 2 ? argv[2] : "");
            return 0;
          }
        else if ((opt == "-q") || (opt == "--quiet"))
          {
            Factory::Instance ().SetVerbose (false);
          }
        else
          {
            break;
          }
      }

    const char *all[] = {"All"};
    const char **test = all;
    int num = 1;
    if (opti < argc)
      {
        test = &argv[opti];
        num = argc - opti;
      }

    for (int i = 0; i < num; ++i)
      {
        try
          {
            Factory::Instance ().Run (test[i]);
          }
        catch (const std::exception &e)
          {
            std::clog << e.what () << std::endl;
          }
      }

    Factory::Instance ().Report ();
    return static_cast<int> (Factory::Instance ().Fail ());
  }
};

class failure : public std::exception
{
  std::string failure_;

public:
  template <typename Left, typename Right>
  failure (const Left &a, Right &b, const char *at = "", const char *expr = "")
  {
    std::ostringstream os;
    os << at << expr << "\nleft:  (" << demangle<Left> () << ") " << a
       << "\nright: (" << demangle<Right> () << ") " << b;
    failure_ = os.str ();
  }
  failure (const char *at = "", const char *expr = "")
  {
    std::ostringstream os;
    os << at << expr;
    failure_ = os.str ();
  }
  virtual ~failure () throw () {}
  const char *what () const throw () override { return failure_.c_str (); }
};

template <typename Suite, typename Case>
struct Registrator
{
  Registrator () { Factory::Instance ().Register (TestName (), Create); }
  const std::string &TestName ()
  {
    static const std::string name (demangle<Suite> ()
                                   + "::" + demangle<Case> ());
    return name;
  }
  static ITest *Create () { return new Case; }
};

template <typename Case>
struct Registrator<Case, void>
{
  Registrator () { Factory::Instance ().Register (TestName (), Create); }
  const std::string &TestName ()
  {
    static const std::string name ("::" + demangle<Case> ());
    return name;
  }
  static ITest *Create () { return new Case; }
};

template <typename Suite, typename Case = void>
struct Test : public ITest, public virtual Suite
{
  static Registrator<Suite, Case> s_Registrator;
  Test ()
    : Suite ()
  {
    Registrator<Suite, Case> *r = &s_Registrator;
    (void) r;
  }
  template <typename E, typename T>
  void assert_throw (void (T::*mf) (), const char *at)
  {
    try
      {
        (dynamic_cast<T *> (this)->*mf) ();
        throw yaffut::failure (at, "statement failed to throw");
      }
    catch (const E &)
      {
      }
  }
};

template <typename Suite, typename Case>
Registrator<Suite, Case> Test<Suite, Case>::s_Registrator;

template <typename Case>
struct Test<Case, void> : public ITest
{
  static Registrator<Case, void> s_Registrator;
  Test ()
  {
    Registrator<Case, void> *r = &s_Registrator;
    (void) r;
  }
  template <typename E, typename T>
  void assert_throw (void (T::*mf) (), const char *at)
  {
    try
      {
        (dynamic_cast<T *> (this)->*mf) ();
        throw yaffut::failure (at, "statement failed to throw");
      }
    catch (const E &)
      {
      }
  }
};

template <typename Case>
Registrator<Case, void> Test<Case, void>::s_Registrator;

template <typename Expected, typename Actual>
constexpr void
equal (const Expected &e, const Actual &a, const char *at = "",
       const char *expr = "")
{
  // We're checking operator==.  Don't assume equivalence with !operator!=
  if (!(e == a))
    {
      throw failure (e, a, at, expr);
    }
}
inline void
equal (double e, double a, const char *at = "", const char *expr = "")
{
  if (std::isfinite (e) && std::isfinite (a))
    {
      double max = std::abs (std::max (e, a));
      max = max < 1.0 ? 1.0 : max;
      if (std::abs (e - a) <= std::numeric_limits<double>::epsilon () * max)
        {
          return;
        }
    }
  else if (e == a)
    {
      return;
    }

  throw failure (e, a, at, expr);
}
constexpr void
check (bool b, const char *at = "", const char *expr = "")
{
  if (!b)
    {
      throw failure (at, expr);
    }
}

template <typename Expected, typename Actual>
constexpr void
unequal (const Expected &e, const Actual &a, const char *at = "",
         const char *expr = "")
{
  // We're checking operator!=.  Don't assume equivalence with !operator==.
  if (!(e != a))
    {
      throw failure (e, a, at, expr);
    }
}
inline void
unequal (double e, double a, const char *at = "", const char *expr = "")
{
  if (std::isfinite (e) && std::isfinite (a))
    {
      double max = std::abs (std::max (e, a));
      max = max < 1.0 ? 1.0 : max;
      if (std::abs (e - a) > std::numeric_limits<double>::epsilon () * max)
        {
          return;
        }
    }
  else if (e != a)
    {
      return;
    }

  throw failure (e, a, at, expr);
}

template <typename T>
void
fail (const T &expr, const char *at = "")
{
  std::ostringstream os;
  os << expr;
  throw failure (at, os.str ().c_str ());
}

template <typename E>
void
assert_throw (void (*pf) (), const char *at = "")
{
  try
    {
      (*pf) ();
      throw failure (at, " statement failed to throw");
    }
  catch (const E &)
    {
    }
}

} // namespace yaffut

//and for those who prefer macro obscurity over typing
#define TEST(Suite, Case)                                                      \
  namespace                                                                    \
  {                                                                            \
  struct Case : public yaffut::Test<Suite, Case>                               \
  {                                                                            \
    Case ();                                                                   \
  };                                                                           \
  }                                                                            \
  template struct yaffut::Test<Suite, Case>;                                   \
  Case::Case ()

#define FUNC(Case)                                                             \
  namespace                                                                    \
  {                                                                            \
  struct Case : public yaffut::Test<Case>                                      \
  {                                                                            \
    Case ();                                                                   \
  };                                                                           \
  }                                                                            \
  template struct yaffut::Test<Case>;                                          \
  Case::Case ()

#ifdef YAFFUT_MAIN

#include <sys/types.h>

int
main (int argc, const char *argv[])
{
  return yaffut::Factory::Instance ().Main (argc, argv);
};

#endif /* YAFFUT_MAIN */

#define yaffut_main(argc, argv) yaffut::Factory::Instance ().Main (argc, argv)

#endif
