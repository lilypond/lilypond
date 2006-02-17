/*
  relocate.cc -- implement relocation based on argv0

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "relocate.hh"

#include "config.hh"

#include <sys/stat.h>
#include <unistd.h>

#if HAVE_GETTEXT
#include <libintl.h>
#endif

#include "file-name.hh"
#include "file-path.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "lily-version.hh"
#include "main.hh"
#include "version.hh"
#include "warn.hh"

#define FRAMEWORKDIR ".."


int
sane_putenv (char const *key, string value, bool overwrite)
{
  if (overwrite || !getenv (key))
    {
      string combine = string (key) + "=" + value;
      char *s = strdup (combine.c_str ());
      return putenv (s);
    }
  
  return -1;
}

static int
set_env_file (char const *key, string value, bool overwrite = false)
{
  if (is_file (value))
    return sane_putenv (key, value, overwrite);
  else if (be_verbose_global)
    warning (_f ("no such file: %s for %s", value, key));
  return -1;
}

static int
set_env_dir (char const *key, string value)
{
  if (is_dir (value))
    return sane_putenv (key, value, false);
  else if (be_verbose_global)
    warning (_f ("no such directory: %s for %s", value, key));
  return -1;
}

static int
prepend_env_path (char const *key, string value)
{
  if (is_dir (value))
    {
      if (be_verbose_global)
	progress_indication (_f ("%s=%s\n", key, value.c_str ())); 

      if (char const *cur = getenv (key))
	value += to_string (PATHSEP) + cur;

      return sane_putenv (key, value.c_str (), true);
    }
  else if (be_verbose_global)
    warning (_f ("no such directory: %s for %s", value, key));
  return -1;
}

string
dir_name (string const file_name)
{
  string s = file_name;
  replace_all (s, '\\', '/');
  ssize n = s.length ();
  if (n && s[n - 1] == '/')
    s[n - 1] = 0;
  s = s.substr (0, s.rfind ('/'));
  return s;
}

#ifdef __MINGW32__
#include <winbase.h>
#endif

void
prefix_relocation (string prefix)
{
  if (be_verbose_global)
    warning (_f ("Relocation: compile prefix=%s, new prefix=%s",
		 prefix_directory,
		 prefix.c_str ()));
  
  string bindir = prefix + "/bin";
  string datadir = prefix + "/share";
  string localedir = datadir + "/locale";
  string lilypond_datadir = datadir + "/lilypond/";

  if (is_dir (lilypond_datadir + "/" + TOPLEVEL_VERSION))
    prefix_directory = lilypond_datadir + "/" + TOPLEVEL_VERSION;
  else if (is_dir (lilypond_datadir + "/current"))
    prefix_directory = lilypond_datadir + "/current";

#if HAVE_GETTEXT
  if (is_dir (localedir))
    bindtextdomain ("lilypond", localedir.c_str ());
#endif

  prepend_env_path ("PATH", bindir);
}

void
framework_relocation (string prefix)
{
  if (be_verbose_global)
    warning (_f ("Relocation: framework_prefix=%s", prefix));

  string bindir = prefix + "/bin";
  string datadir = prefix + "/share";
  string libdir = prefix + "/lib";
  string sysconfdir = prefix + "/etc";

  /* need otherwise dynamic .so's aren't found.   */
  prepend_env_path ("DYLD_LIBRARY_PATH", libdir);
  
  set_env_file ("FONTCONFIG_FILE", sysconfdir + "/fonts/fonts.conf", true);
  set_env_dir ("FONTCONFIG_PATH", sysconfdir + "/fonts");

#ifdef __MINGW32__
  char font_dir[PATH_MAX];
  ExpandEnvironmentStrings ("%windir%/fonts", font_dir, sizeof (font_dir));
  prepend_env_path ("GS_FONTPATH", font_dir);
#endif

  string gs_version =
#ifdef GHOSTSCRIPT_VERSION
    GHOSTSCRIPT_VERSION
#else
    "ghostscript-version-undefined"
#endif
    ;
  
  if (char const *cur = getenv ("LILYPOND_GS_VERSION"))
    gs_version = cur;
  
  prepend_env_path ("GS_FONTPATH", datadir + "/ghostscript/" + gs_version + "/fonts");
  prepend_env_path ("GS_LIB", datadir + "/ghostscript/" + gs_version + "/Resource");
  prepend_env_path ("GS_LIB", datadir + "/ghostscript/" + gs_version + "/lib");

  prepend_env_path ("GS_FONTPATH", datadir + "/gs/fonts");
  prepend_env_path ("GS_LIB", datadir + "/gs/Resource");
  prepend_env_path ("GS_LIB", datadir + "/gs/lib");

  prepend_env_path ("GUILE_LOAD_PATH", datadir
		    + to_string ("/guile/%d.%d",
				 SCM_MAJOR_VERSION, SCM_MINOR_VERSION));

  set_env_file ("PANGO_RC_FILE", sysconfdir + "/pango/pangorc");
  set_env_dir ("PANGO_PREFIX", prefix);
  
  prepend_env_path ("PATH", bindir);
}

string
get_working_directory ()
{
  char cwd[PATH_MAX];
  getcwd (cwd, PATH_MAX);

  return string (cwd);
}

void
setup_paths (char const *argv0_ptr)
{
  File_name argv0_filename (argv0_ptr);
  
  prefix_directory = LILYPOND_DATADIR;
  if (relocate_binary
      && getenv ("LILYPOND_RELOCATE_PREFIX"))
    {
      string prefix = getenv ("LILYPOND_RELOCATE_PREFIX");
#ifdef __MINGW32__
      /* Normalize file name.  */
      prefix = File_name (prefix).to_string ();
#endif /* __MINGW32__ */
      prefix_relocation (prefix);
      string bindir = prefix + "/bin";
      framework_relocation (bindir);
    }
  else if (relocate_binary)
    {
      string argv0_abs;
      if (argv0_filename.is_absolute ())
	{
	  argv0_abs = argv0_filename.to_string ();
	  if (be_verbose_global)
	    warning (_f ("Relocation: is absolute: argv0=%s", argv0_ptr));
	}
      else if (argv0_filename.dir_.length ())
	{
	  argv0_abs = get_working_directory ()
	    + "/" + string (argv0_filename.to_string ());
	  if (be_verbose_global)
	    warning (_f ("Relocation: from cwd: argv0=%s", argv0_ptr));
	}
      else
	{
	  /* Find absolute ARGV0 name, using PATH.  */
	  File_path path;
	  path.parse_path (getenv ("PATH"));

	  if (be_verbose_global)
	    warning (_f ("Relocation: from PATH=%s\nargv0=%s",
			 path.to_string ().c_str (), argv0_ptr));

#ifndef __MINGW32__
	  argv0_abs = path.find (argv0_filename.to_string ());
#else /* __MINGW32__ */
	  char const *ext[] = {"exe", "", 0 };
	  argv0_abs = path.find (argv0_filename.to_string (), ext);
#endif /* __MINGW32__ */

	  if (argv0_abs.empty ())
	    programming_error ("can't find absolute argv0.");
	}

      string bindir = dir_name (argv0_abs);
      string argv0_prefix = dir_name (bindir);
      string compile_prefix = dir_name (dir_name (dir_name (prefix_directory)));
      if (argv0_prefix != compile_prefix)
	prefix_relocation (argv0_prefix);
      if (argv0_prefix != compile_prefix || string (FRAMEWORKDIR) != "..")
	framework_relocation (bindir + "/" + FRAMEWORKDIR);
    }

  /* FIXME: use LILYPOND_DATADIR.  */
  if (char const *env = getenv ("LILYPONDPREFIX"))
    {

#ifdef __MINGW32__
      /* Normalize file name.  */
      prefix_directory = File_name (env).to_string ();
#else
      prefix_directory = env;
#endif
    }

  global_path.append ("");


  /*
    When running from build dir, a full LILYPOND_PREFIX is set-up at

        $(OUTBASE)/share/lilypond/TOPLEVEL_VERSION

     This historical hack will allow the shorthand

        LILYPONDPREFIX=out lily/out/lilypond ...

  */
  
  struct stat statbuf;
  string build_prefix_current = prefix_directory + "/share/lilypond/" "current";
  string build_prefix_version = prefix_directory + "/share/lilypond/" TOPLEVEL_VERSION;
  if (stat (build_prefix_version.c_str (), &statbuf) == 0)
    prefix_directory = build_prefix_version;
  else if (stat (build_prefix_current.c_str (), &statbuf) == 0)
    prefix_directory = build_prefix_current;

  
  /* Adding mf/out make lilypond unchanged source directory, when setting
     LILYPONDPREFIX to lilypond-x.y.z */
  char const *suffixes[] = {"ly", "ps", "scm", 0 };

  
  vector<string> dirs;
  for (char **s = suffixes; *s; s++)
    {
      string path = prefix_directory + to_string ('/') + string (*s);
      dirs.push_back (path);
    }


  dirs.push_back (prefix_directory + "/fonts/otf/");
  dirs.push_back (prefix_directory + "/fonts/type1/");
  dirs.push_back (prefix_directory + "/fonts/svg/");
  
  for (vsize i = 0; i < dirs.size (); i++)
    global_path.prepend (dirs[i]);
}
