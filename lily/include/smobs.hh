/*
  smobs.hh -- declare smob related stuff.

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SMOBS_HH
#define SMOBS_HH

#include "lily-guile.hh"
#include "warn.hh"

/*
  Smobs are GUILEs mechanism of exporting C(++) objects to the Scheme
  world.  They are documented in the GUILE manual.


  In LilyPond, smobs are created from C++ objects through macros.
  There are two types of smob objects.

  1. Simple smobs are intended for simple objects like numbers:
  immutable objects that can be copied without change of meaning.

  To obtain an SCM version of a simple smob, use the member function
  SCM smobbed_copy ().

  Simple smobs are created by adding the
  DECLARE_SIMPLE_SMOBS(Classname) to the declaration

  2. Complex smobs are objects that have an identity. These objects
  carry this identity in the form of a self_scm () method, which is a
  SCM pointer to the object itself.

  The constructor for a complex smob should have 3 steps:

  * initialize all SCM members to a non-immediate value (like SCM_EOL)

  * call smobify_self ()

  * initialize SCM members

  For example,

  Complex_smob::Complex_smob () {
  scm_member_ = SCM_EOL;
  smobify_self ();
  scm_member_ = <..what you want to store..>
  }

  after construction, the self_scm () field of a complex smob is
  protected from Garbage Collection.  This protection should be
  removed once the object is put into another (reachable) Scheme data
  structure, i.e.

  Complex_smob *p = new Complex_smob;
  list = scm_cons (p->self_scm (), list);
  scm_gc_unprotect_object (p->self_scm ());

  Complex smobs are made with DECLARE_SMOBS (Classname) in the class
  declaration.

  CALLING INTERFACE

  Common public methods to C++ smob objects:

  unsmob (SCM x)  - unpacks X and returns pointer to the C++ object, or 0
  if it has the wrong type.

  SCM equal_p (SCM a, SCM b) - compare A and B. Returns a Scheme boolean


  IMPLEMENTATION

  For implementating a class, the following should be provided

  - an equal_p () function (a default is in the
  IMPLEMENT_DEFAULT_EQUAL_P macro in ly-smobs.icc)

  - mark_smob () function, that calls scm_gc_mark () on all Scheme
  objects in the class

  - a print_smob () function, that displays a representation for
  debugging purposes

  - A call to one of the IMPLEMENT_SMOBS or IMPLEMENT_SIMPLE_SMOBS macros
  from file "ly-smobs.icc"
*/

#define DECLARE_SIMPLE_SMOBS(CL)		\
  public:					\
  SCM smobbed_copy () const;			\
  DECLARE_BASE_SMOBS (CL)

#define DECLARE_BASE_SMOBS(CL)					\
  friend class Non_existent_class;				\
  private:							\
  static const char* smob_name_; \
  static scm_t_bits smob_tag_;					\
  static SCM mark_smob (SCM);					\
  static size_t free_smob (SCM s);				\
  static int print_smob (SCM s, SCM p, scm_print_state*);	\
  public:							\
  static SCM equal_p (SCM a, SCM b);				\
  static CL *unsmob (SCM s) __attribute__((pure))		\
  {								\
    if (SCM_NIMP (s) && SCM_CELL_TYPE (s) == smob_tag_)		\
      return (CL *) SCM_CELL_WORD_1 (s);			\
    else							\
      return 0;							\
  }								\
  static SCM smob_p (SCM);					\
  static void init_smobs ();					\
  private:

#define DECLARE_SMOBS(CL)			\
  DECLARE_BASE_SMOBS (CL)			\
    protected:					\
  virtual ~CL ();				\
  SCM unprotected_smobify_self ();		\
  private:					\
  void smobify_self ();				\
  SCM self_scm_;				\
  SCM protection_cons_;				\
  public:					\
  SCM unprotect ();				\
  void protect ();				\
  SCM self_scm () const { return self_scm_; }	\
  private:

#define DECLARE_UNSMOB(CL, name)		\
  inline CL *					\
  unsmob_ ## name (SCM s)			\
  {						\
    return CL::unsmob (s);			\
  }

#define DECLARE_TYPE_P(CL) extern SCM CL ## _type_p_proc

void protect_smob (SCM smob, SCM *prot_cons);
void unprotect_smob (SCM smob, SCM *prot_cons);

extern bool parsed_objects_should_be_dead;

#ifndef NDEBUG
#define ASSERT_LIVE_IS_ALLOWED()     \
  static bool passed_here_once;\
  if (parsed_objects_should_be_dead && !passed_here_once) { \
    ::programming_error (string ("Parsed object should be dead: ")  + __PRETTY_FUNCTION__ ); \
    passed_here_once = true;\
  }    
#else
#define ASSERT_LIVE_IS_ALLOWED()
#endif

#endif /* SMOBS_HH */

