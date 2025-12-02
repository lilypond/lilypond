#!/usr/bin/env perl

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2019--2025 Werner Lemberg <wl@gnu.org>
#
# LilyPond is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


# This Perl script needs a few modules that are not core modules:
#
#   File::Which
#   IPC::Run3
#   Pandoc
#   Parallel::ForkManager
#
# Either install missing modules using your package manager (if
# available) or use `cpanm` to download, compile, and install them.


use 5.14.0; # We use `s///r`.
use strict;
use warnings;

use Cwd qw(abs_path); # For file name handling.
use Encode qw(decode);
use File::Basename qw(fileparse); # For file name handling.
use File::Spec::Functions qw(:ALL); # For file name manipulation.
use File::Which; # For checking the existence of `convert-ly`.
use Getopt::Long qw(:config no_ignore_case
                            bundling_values); # For option handling.
use HTTP::Tiny; # For accessing the MediaWiki API.
use IPC::Run3; # For communication with `convert-ly`.
use JSON::PP; # For parsing the data returned by the MediaWiki API.
use Pandoc; # Accessing `pandoc`.
use Parallel::ForkManager; # For parallel processing.
use Text::Wrap; # For formatting the 'category' header field.


# Get the top directory of the LilyPond git repository...
my $script_dir = (fileparse(abs_path(__FILE__)))[1];
my @script_dir_components = splitdir($script_dir);
pop @script_dir_components for 1..3; # Remove `scripts/auxiliar/`.
my $top_dir = catdir(@script_dir_components);

# ... so that we can parse file `VERSION`.
my $version_file = catdir($top_dir, "VERSION");
open(my $vfh, "<", $version_file)
  or die "error: Can't open '$version_file': $!\n";
read $vfh, my $version_strings, -s $vfh;
close($vfh) or warn "warning: Can't close file '$version_file': $!\n";

my $STABLE_MAJOR;
my $STABLE_MINOR;
my $STABLE_MICRO;

if ($version_strings =~
    /
      ^ \h* VERSION_STABLE \h* =
          \h* ([0-9]+) \. ([0-9]+) \. ([0-9]+) \h* $
    /xm) {
  $STABLE_MAJOR = $1;
  $STABLE_MINOR = $2;
  $STABLE_MICRO = $3;
}
else {
  die<<"EOT";
error: Can't find variable 'VERSION_STABLE' in file

    $version_file

  to get the current stable LilyPond version
EOT
}


# Don't buffer 'stdout'.
$| = 1;


# Function:
#   Show help text and exit.
#
# Arguments:
#   $ret    Exit code.
#
sub show_help {
  my ($ret) = @_;

  my $help = <<'EOT';
Usage: makelsr.pl [OPTION]...
Use the LilyPond Wiki to regenerate LilyPond's 'snippets' directory.

  -a, --all              dump all snippets contained in the LilyPond
                           Wiki, not only the documentation ones (this
                           needs '--dump')
  -A, --all-dump-only=FILE
                         shorthand for '--all --no-convert --no-lsr
                           --no-new --no-snippet-list --dump=FILE'
  -c, --no-convert       don't call 'convert-ly'
  -d, --dump=FILE        dump snippet data from the LilyPond Wiki into
                           FILE
  -D, --diff-version-update
                         only update '\version' number of a snippet if
                           it is modified
  -f, --no-snippet-list  don't update snippet list files
  -h, --help             display this help and exit
  -i, --input=FILE       use FILE as input (as saved with '--dump')
                           instead of downloading data from the
                           LilyPond Wiki
  -j, --jobs=N           use N jobs for creating snippet files in
                           parallel (default: 1)
  -l, --no-lsr           don't download snippets from the LilyPond Wiki
                           to update the 'snippets' directory
  -n, --no-new           don't use snippets from the 'new'
                           subdirectory to update the 'snippets'
                           directory
  -N, --new              the same as '--no-lsr', but also suppress
                           'new category' warnings
  -p, --path=DIR         directory where to look for the 'convert-ly'
                           script
  -s, --top-source=DIR   directory where to look for subdirectory
                           'Documentation/snippets' (default: '.')
  -V, --verbose          be verbose

Environment variable:
  LILYPOND_BUILD_DIR     top of the LilyPond build directory tree


This script performs the following actions; use the above options to
modify them.

* Download LilyPond Wiki snippets from
  'https://wiki.lilypond.community' (this Wiki is the successor of the
  LSR, the LilyPond Snippet Repository, previously hosted at
  'https://lsr.di.unimi.it').

* Delete all snippet and snippet list files in directory
  'Documentation/snippets' (but not in 'Documentation/snippets/new').

* Use all snippets from the LilyPond Wiki that are part of the category
  'Included in the official documentation', convert their documentation
  parts from MediaWiki syntax to Texinfo with the 'pandoc' program, run
  the script 'convert-ly' to update their LilyPond code parts to
  current syntax, and store them in 'Documentation/snippets'.

  The snippet file names are derived from the snippet titles.
  Additionally, snippet list files with the extension '.snippet-list'
  are created that list the snippets grouped by the categories assigned
  in the Wiki.

* Convert all snippet files in 'Documentation/snippets/new' with
  'convert-ly' and output them to 'Documentation/snippets', possibly
  overwriting existing files.

The 'convert-ly' script is searched in the following order; the first
successful hit wins.

* In the directory given with option '--path'.
* In directory '$LILYPOND_BUILD_DIR/out/bin'.
* In directory './build/out/bin'.
* In $PATH.
EOT

  print $help;
  exit $ret;
}


# Option handling.
my $all = 0;
my $all_dump_only = "";
my $diff_version_update = 0;
my $dump = "";
my $help = 0;
my $input = "";
my $jobs = 1;
my $new = 0;
my $no_convert = 0;
my $no_lsr = 0;
my $no_new = 0;
my $no_snippet_list = 0;
my $path = "";
my $top_source = ".";
my $verbose = 0;

GetOptions(
  "all|a" => \$all,
  "all-dump-only=s" => \$all_dump_only,
  "diff-version-update|D" => \$diff_version_update,
  "dump|d=s" => \$dump,
  "help|h" => \$help,
  "input|i=s" => \$input,
  "jobs|j=i" => \$jobs,
  "new|N" => \$new,
  "no-convert|c" => \$no_convert,
  "no-lsr|l" => \$no_lsr,
  "no-new|n" => \$no_new,
  "no-snippet-list|f" => \$no_snippet_list,
  "path|p=s" => \$path,
  "top-source|s=s" => \$top_source,
  "verbose|V" => \$verbose,
) || show_help(2);
show_help(1) if $help;

if ($new) {
  $no_lsr = 1;
}

if ($all_dump_only) {
  $all = 1;
  $dump = $all_dump_only;
  $no_convert = 1;
  $no_lsr = 1;
  $no_new = 1;
  $no_snippet_list = 1;
}


# Check availability of external programs.
unless ($no_lsr) {
  pandoc or die "error: 'pandoc' executable not found\n";
}

my $CONVERT_LY = "";
unless ($no_convert) {
  if ($path) {
    $CONVERT_LY = catfile($path, "convert-ly");
    if (!-x $CONVERT_LY) {
      warn "warning: Option '--path' set"
           . " but can't use '$CONVERT_LY'\n";
      $CONVERT_LY = "";
    }
  }
  if (!$CONVERT_LY) {
    if (defined $ENV{"LILYPOND_BUILD_DIR"}) {
      $CONVERT_LY = catfile($ENV{"LILYPOND_BUILD_DIR"},
                            "out", "bin", "convert-ly");
      if (!-x $CONVERT_LY) {
        warn   "warning: Environment variable"
               . " \$LILYPOND_BUILD_DIR is set\n"
             . "  but can't use '$CONVERT_LY'\n";
        $CONVERT_LY = "";
      }
    }
    else {
      print "'LILYPOND_BUILD_DIR' environment variable not set\n"
        if $verbose;
    }
  }
  if (!$CONVERT_LY) {
    $CONVERT_LY = catfile("build", "out", "bin", "convert-ly");
    if (!-x $CONVERT_LY) {
      print "Can't use '$CONVERT_LY'\n" if $verbose;
      $CONVERT_LY = "";
    }
  }
  if (!$CONVERT_LY) {
    $CONVERT_LY = which "convert-ly";
    if (!defined $CONVERT_LY) {
      print "Can't find 'convert-ly' in path\n" if $verbose;
      die "error: 'convert-ly' script not found\n";
    }
  }

  print "Using '$CONVERT_LY'\n" if $verbose;
}


# Check the actual snippet directories.
my $snippet_dir = catdir("Documentation", "snippets");
my $new_snippet_dir = catdir($snippet_dir, "new");

unless ($no_lsr && $no_snippet_list && $no_new) {
  my @dirs;

  push @dirs, catdir($top_source, $snippet_dir);
  push @dirs, catdir($top_source, $new_snippet_dir) unless $no_new;

  for my $dir (@dirs) {
    die "error: No directory '$dir'\n" unless -d $dir;
    die "error: Directory '$dir' is not readable\n" unless -r $dir;
    die "error: Directory '$dir' is not writable\n" unless -w $dir;
  }
}


# Change to the top source directory for simplicity.
my $curr_dir = rel2abs(curdir);
chdir $top_source
  or die "error: Can't change to directory '$top_source'\n";


# Clean up.
unless ($no_lsr) {
  print "Removing old snippet files\n";
  while (my $f = glob(catfile($snippet_dir, "*.ly"))) {
    unlink $f or warn "warning: Can't remove file '$f': $!\n";
  }
}

unless ($no_snippet_list || $no_lsr) {
  print "Removing old snippet list files\n";
  while (my $f = glob(catfile($snippet_dir, "*.snippet-list"))) {
    unlink $f or warn "warning: Can't remove file '$f': $!\n";
  }
}


# Two categories that we are handling specially.
my $DOC_CATEGORY_NEW = "Included in the official documentation";
my $DOC_CATEGORY = "Category:" . $DOC_CATEGORY_NEW;
my $SNIPPET_CATEGORY_NEW = "Snippet";
my $SNIPPET_CATEGORY = "Category:" . $SNIPPET_CATEGORY_NEW;

# A set of all categories used by documentation snippets.
my %wiki_categories;

# The snippets indexed by the title.  Elements of this hash have the
# following form:
#
#   [<page>, <version>, <doc>, <code>, <filename>, {<categories>}]
#
# For snippets in the Wiki, `<version>`, `<doc>`, and `<code>` are
# results from parsing `<page>`; `<filename>` is derived from the
# title.
#
# Snippets from the `new` directory have an undefined `<page>` field
# and a `<version>` that is larger than the stable release, indicating
# that `<doc>` is already in Texinfo format.
my %snippets;

# Titles of snippets that don't follow the expected structure.
my @invalid_snippets;


# Function:
#   Check whether the given version equals the current stable version.
#
# Arguments:
#   $version_string    Version string in the format 'X.Y' or 'X.Y.Z'.
#
# Return:
#   1 if true, else 0.
#
sub is_current_stable_version {
  my ($version_string) = @_;
  my ($major, $minor, $micro) = split /\./, $version_string;
  my $ret;

  $ret = $major == $STABLE_MAJOR && $minor == $STABLE_MINOR;
  $ret = $micro <= $STABLE_MICRO if $ret && defined $micro;

  return $ret;
}


# Function:
#   Split a LilyPond Wiki page into documentation and LilyPond code.
#
# Arguments:
#   $page    The Wiki page.
#
# Return:
#   A three-element list giving the LilyPond version, the documentation
#   block, and the LilyPond code block.
#
sub parse_wiki_page {
  my ($page) = @_;
  my $version = undef;
  my $doc = undef;
  my $code = undef;

  # The 'full' attribute is not needed for the snippet import.
  if ($page =~
      /
        \A
        \s* (?<doc> (?s) .*? ) \s* \n

        \s* <lilypond
          (?: \s+ full )?
          \s+ version \s* = \s* " (?<version> [0-9.]+ ) "
          (?: \s+ full )?
          \s* > \s* \n
            \s* (?<code> (?s) .*? ) \s* \n
        \s* <\/lilypond \s* > \s* \n

        \s* (?: \[\[Category:.*?\]\] \s* )+
        \z
      /xm) {
    $version = $+{"version"};
    $doc = $+{"doc"};
    $code = $+{"code"};
  }

  return ($version, $doc, $code);
}


# Function:
#   Convert MediaWiki page titles to file names, similar to the code in
#   the original Java implementation used to extract LSR snippets.
#
# Arguments:
#   $s              The string to be manipulated.
#
# Return:
#   The converted string.
#
sub title_to_filename {
  my ($s) = @_;

  # Translate some characters not generally allowed in file names.  We
  # don't have to take care of characters from the set '#<>[]|{}' since
  # they aren't allowed in MediaWiki page titles.
  $s =~ tr[* /:?_;\\]
          [+\-\-\-\-\-\-\-];

  # To have portable file names, all non-ASCII characters are
  # translated, too.
  $s =~ tr/\x00-\x7F/-/c;

  # Remove some problematic characters entirely.
  $s =~ s/[()"']+//g;

  # Convert to lowercase.
  $s = lc($s);

  return $s;
}


# Function:
#   Open and parse a dump file.
#
# Arguments:
#   $filename    The dump file.
#
sub parse_wiki_dump {
  my ($filename) = @_;

  open(my $fh, "<:encoding(UTF-8)", $filename)
    or die "error: Can't open dump file '$filename': $!\n";
  read $fh, my $wiki_dump, -s $fh;
  close($fh) or warn "warning: Can't close file '$filename': $!\n";

  my @entries = split /\n={70}\n/m, $wiki_dump;

  foreach my $entry (@entries) {
    # We ignore the 'Filename' field since it is for information only.
    if ($entry =~
        /
          \A
          Title: \s (?<title> .*) \n
          Filename: \s .* \n
          Categories: \n
            (?<categories>
              (?: \s \s Category: .* \n )+
            )
          -------- \n
          (?<page> (?s) .* )
          \z
        /xm) {
      my $title = $+{"title"};
      my %categories = map { s/^  //r => 1 }
                         split /\n/, $+{"categories"};
      my $page = $+{"page"};

      my $version = undef;
      my $doc = undef;
      my $code = undef;

      if (defined $categories{$DOC_CATEGORY}) {
        # Add the snippet's categories to the global set.
        @wiki_categories{keys %categories} = (1) x %categories;

        warn   "warning: Category '$SNIPPET_CATEGORY'"
               . "is missing in snippet with title\n"
             . "  '$title'\n"
          if not defined $categories{$SNIPPET_CATEGORY};

        ($version, $doc, $code) = parse_wiki_page($page);

        # Only take snippets that are valid and compile with the stable
        # version of LilyPond.
        unless (defined $version
                && is_current_stable_version($version)) {
          push @invalid_snippets, $title;
        }
      }

      $snippets{$title} = [$page, $version, $doc, $code,
                           title_to_filename($title) . ".ly",
                           {%categories}];
    }
    else {
      my $first_line = (split /\n/, $entry)[0];
      die   "error: invalid Wiki page starting with\n"
          . "\n"
          . "  $first_line\n";
    }
  }
}


# Function:
#   Download JSON data from LilyPond Wiki.
#
sub download_wiki {
  print "  Downloading from https://wiki.lilypond.community\n"
    if $verbose;

  my $http = HTTP::Tiny->new();

  my $doc_category = $DOC_CATEGORY;
  $doc_category =~ s/ /_/g;
  my $category = $all ? $SNIPPET_CATEGORY : $doc_category;

  # The WikiMedia API returns the data in chunks.  For the
  # 'categorymembers' generator, we have to observe the key
  # 'gcmcontinue' in the 'continue' object.  As long as 'continue' is
  # defined in the received data, use the value of the 'gcmcontinue'
  # key to download the next chunk of data.
  #
  # We access the 'revisions' and the 'categories' properties
  # simultaneously.  To avoid another 'chunkification' loop for
  # categories we explicitly set chunk limits, assuming that there
  # aren't more than 20 categories for a single Wiki snippet page.
  my $request =
       "https://wiki.lilypond.community/api.php"
       . "?format=json"
       . "&formatversion=2"
       . "&action=query"
       . "&generator=categorymembers"
       . "&gcmtitle=" . $category
       . "&gcmlimit=20"
       . "&prop=revisions|categories"
       . "&rvprop=content"
       . "&rvslots=*"
       . "&cllimit=400";
  my $token = "";

  while (1) {
    my $response = $http->get($request . "&gcmcontinue=$token");
    if (!$response->{success}) {
      print   "\n"
            . " failed:\n"
            . "    $response->{status} $response->{reason}\n"
        if $verbose;
      die "error: Can't download data from LilyPond Wiki\n";
    }

    my $json = decode_json $response->{"content"};

    foreach my $entry (@{$json->{"query"}{"pages"}}) {
      # Only use pages from the 'main' namespace (i.e., ignore
      # 'Category:' pages and the like).
      next if $entry->{"ns"} != 0;

      my $title = $entry->{"title"};
      my %categories = map { $_->{"title"} => 1 }
                         @{$entry->{"categories"}};
      my $page = $entry->{"revisions"}[0]{"slots"}{"main"}{"content"};

      my $version = undef;
      my $doc = undef;
      my $code = undef;

      if (defined $categories{$DOC_CATEGORY}) {
        @wiki_categories{keys %categories} = (1) x %categories;

        warn   "warning: Category '$SNIPPET_CATEGORY'"
               . "is missing in snippet with title\n"
             . "  '$title'\n"
          if not defined $categories{$SNIPPET_CATEGORY};

        ($version, $doc, $code) = parse_wiki_page($page);

        unless (defined $version
                && is_current_stable_version($version)) {
          push @invalid_snippets, $title;
        }
      }

      $snippets{$title} = [$page, $version, $doc, $code,
                           title_to_filename($title) . ".ly",
                           {%categories}];
    }

    print $verbose ? " " . scalar keys %snippets
                   : ".";

    last if not defined $json->{"continue"};

    $token = $json->{"continue"}{"gcmcontinue"};
  }

  print " OK\n";
}


# Download snippets or access dump file.
print "Getting snippets from LilyPond Wiki...";
print "\n" if $verbose;

if ($input) {
  my $wiki_dump = file_name_is_absolute($input)
                    ? $input
                    : catfile($curr_dir, $input);
  print "\n" if not $verbose;
  print "  Using LilyPond Wiki dump file '$wiki_dump'\n" if $verbose;

  parse_wiki_dump($wiki_dump);
}
else {
  download_wiki();
}


# Function:
#   Subroutine for `sort`, ignoring a `.ly` file extension while
#   comparing file names.  This makes 'foo-bar-baz.ly' be sorted after
#   'foo-bar.ly'.
#
# Return:
#   Result of `cmp`.
#
sub filename_sort {
  my ($aa, $bb) = map { s/\.ly$//r } ($a, $b);

  return $aa cmp $bb;
}


# Function:
#   Same as `filename_sort` but for `%snippets`.
#
sub filename_sort2 {
  my ($aa, $bb) = map { s/\.ly$//r }
                    ($snippets{$a}->[4], $snippets{$b}->[4]);

  return $aa cmp $bb;
}


# Function:
#   Clean up data entries.
#
# Arguments:
#   $data         The data to be manipulated.
#   $is_string    If set, escape backslashes for usage as a LilyPond
#                 string.
#
# Return:
#   The manipulated data.
#
sub normalize_text {
  my ($data, $is_string) = @_;

  # Backslashes must be doubled in LilyPond strings.
  $data =~ s/\\/\\\\/g if $is_string;

  # Remove trailing (horizontal) whitespace from every line.
  $data =~ s/\h+$//gm;

  # Remove leading and trailing empty lines.
  $data =~ s/^\n+//;
  $data =~ s/\n+$//;

  return $data;
}


# Write a dump of the LilyPond Wiki.
if ($dump) {
  my $dump_file = file_name_is_absolute($dump)
                    ? $dump
                    : catfile($curr_dir, $dump);

  print "Writing LilyPond Wiki dump";
  print " to file '$dump_file'" if $verbose;
  print "\n";

  open(my $fh, ">:encoding(UTF-8)", $dump_file)
    or die "error: Can't open file '$dump_file': $!\n";

  # We emit the snippets sorted by title, including the file name for
  # convenience (i.e., for information only; the file names are always
  # computed from the actual title while writing the snippet files).
  foreach my $title (sort keys %snippets) {
    print $fh "Title: " . $title . "\n";
    print $fh "Filename: " . title_to_filename($title) . ".ly\n";
    print $fh "Categories:\n";
    print $fh map { "  " . $_ . "\n" }
                sort keys %{$snippets{$title}[5]};
    print $fh "--------\n";
    print $fh $snippets{$title}[0] . "\n";
    print $fh "=" x 70 . "\n";
  }

  close($fh) or warn "warning: Can't close file '$dump_file': $!\n";
}


# Disable invalid snippets and warn about them.
if (@invalid_snippets && !($no_lsr || $no_snippet_list)) {
  foreach my $title (@invalid_snippets) {
    delete $snippets{$title}->[5]{$DOC_CATEGORY}
  }

  my $title_strings = join "\n", map { "    " . $_ } @invalid_snippets;
  my ($snip, $tit, $donot, $have) =
       @invalid_snippets > 1 ? ("snippets", "titles", "don't", "have")
                             : ("snippet", "title", "doesn't", "has");
  my $x_y_z = "$STABLE_MAJOR.$STABLE_MINOR.$STABLE_MICRO";
  my $x_y = "$STABLE_MAJOR.$STABLE_MINOR";

  warn <<"EOT";
warning: The $snip with the $tit

$title_strings

  $donot follow the expected structure or $have the wrong version
  (i.e., not "$x_y" or "$x_y_z") and are thus ignored:

    documentation text

    <lilypond version="...">
      LilyPond code
    </lilypond>

    [[Category:...]]
    [[Category:...]]
    ...
EOT
}


# Handle new snippets.  We expect them to have a structure similar to
# snippets derived from the LilyPond Wiki.
unless ($no_new) {
  print "Getting snippets from subdirectory 'new'\n";
  opendir my $dh, $new_snippet_dir
    or die "error: Can't open directory '$new_snippet_dir': $!\n";

  my %new_categories;

  for my $f (sort filename_sort readdir $dh) {
    next unless $f =~ /\.ly$/;
    my $filename = catfile($new_snippet_dir, $f);
    next unless -f $filename;

    # Read snippet.
    print "  $f\n" if $verbose;
    open(my $fh, "<:encoding(UTF-8)", $filename)
      or die "error: Can't open file '$filename': $!\n";
    read $fh, my $content, -s $fh;
    close($fh) or warn "warning: Can't close file '$filename': $!";

    # Split content into fields similar to what LilyPond Wiki snippets
    # are split into after being processed with `pandoc`.
    my $version;
    my %categories;
    my $texidoc;
    my $doctitle;
    my $code;

    # We use named groups `(?<name> ...)` in the regular expression,
    # also removing leading and trailing whitespace.  `(?: ...)` is for
    # clustering, `*?` is the non-greedy version of `*`, and `(?s)`
    # makes `.` match newlines, too.  Finally, `\A` and `\z` match the
    # start and the end of the string, respectively, while `$` matches
    # EOL because of the 'm' modifier of this regex.
    #
    # The hash called `+` holds all matches of named groups.
    if ($content =~
        /
          \A \s*

          \\version \s* " (?<version> [0-9.]+ ) " \s*

          \\header \s* { \s*

            categories \s* = \s* " \s*
              (?<categories> (?: \\" | [^"] )*? )
            \s* " \s*

            texidoc \s* = \s* " \s*
              (?<texidoc> (?: \\" | [^"] )*? )
            \s* " \s*

            doctitle \s* = \s* " \s*
              (?<doctitle> (?: \\" | [^"] )*? )
            \s* " \s*

          } \h* (?<invalid> .*? ) \h* $ \s*

          (?<code> (?s) .*? )

          \s* \z
        /xm) {
      my $categories_string = $+{"categories"};
      $version = $+{"version"};
      $texidoc = $+{"texidoc"};
      $doctitle = $+{"doctitle"};
      $code = $+{"code"};

      $categories_string =~ s/^\s*//;
      $categories_string =~ s/\s*$//;
      $categories_string =~ s/\s+/ /g;
      %categories = map { s/^/Category:/r => 1 }
                      split /\s*,\s*/, $categories_string;

      # Some data checks.

      warn   "warning: Using a stable version in new snippet\n"
           . "  '$filename'\n"
        if is_current_stable_version($version);

      # This is to catch '% begin verbatim', which gets automatically
      # inserted by this script.
      die <<"EOT"
error: In snippet '$filename',
  the closing bracket of the '\\header' block must be
  the last character on this line (found '$+{"invalid"}')
EOT
        if $+{"invalid"};

      # While there is basically no limit of categories in the Wiki, we
      # warn if there are more entries than an ad-hoc threshold.
      my $limit = 6;
      warn   "warning: More than $limit categories in snippet\n"
           . "  '$filename'\n"
        if %categories > $limit;
      if (defined $categories{$DOC_CATEGORY}
          || defined $categories{$SNIPPET_CATEGORY}) {
        die <<"EOT"
warning: Don't set categories

    $DOC_CATEGORY_NEW
    $SNIPPET_CATEGORY_NEW

  in snippet

    $filename
EOT
      }
      else {
        $categories{$DOC_CATEGORY} = 1;
        $categories{$SNIPPET_CATEGORY} = 1;
      }
      die   "warning: No category set in snippet\n"
          . "  '$filename'\n"
        if %categories == 2;
      foreach my $category (sort keys %categories) {
        if (!defined $wiki_categories{$category}) {
          my $c = $category;
          $c =~ s/^Category://;
          warn   "warning: New category '$c' in snippet\n"
               . "  '$filename'\n" if not $new;
          $new_categories{$category} = 1;
        }
      }

      my $t = $texidoc;
      $t =~ s/\\\\//g;
      $t =~ s/\\"//g;
      die   "Unescaped backslash found in 'texidoc' field of file\n"
          . "  '$filename'\n"
        if $t =~ /\\/;

      # No newlines in title.
      $doctitle =~ s/\n/ /g;
      $doctitle =~ s/\\\\/\\/g;

      my $d = title_to_filename($doctitle) . ".ly";
      die <<"EOT"
error: 'doctitle' field in snippet '$filename'
  doesn't map to its file name:
    expected '$f'
    but got  '$d'
EOT
        if $d ne $f;
    }
    else {
      die <<"EOT";
error: Snippet '$filename'
  doesn't follow the expected structure:

    \\version "..."

    \\header {
      categories = "..."
      texidoc = "..."
      doctitle = "..."
    }

    <lilypond code>
EOT
    }

    # Create (or overwrite) database entry.
    $snippets{$doctitle} = [undef, $version, $texidoc, $code, $f,
                            {%categories}];
  }

  %wiki_categories = (%wiki_categories, %new_categories);

  closedir $dh;
}


# Check whether there are ambiguous file names.
unless ($no_lsr && $no_snippet_list) {
  # A map from file names to titles.
  my %titles;

  foreach my $title (keys %snippets) {
    my $snippet = $snippets{$title};
    my $filename = $snippet->[4];

    next if not defined $snippet->[5]{$DOC_CATEGORY};

    if (defined $titles{$filename}) {
      die <<"EOT"
error: The two titles

    $title
    $titles{$filename}

  map to the same file name

    $filename
EOT
    }
    else {
      $titles{$filename} = $title;
    }
  }
}


# Write snippet list files.  No need to parallelize this since it is
# very quick.
unless ($no_snippet_list) {
  # A hash (with categories as keys) of sets (with file names as keys).
  my %snippet_lists;

  # Fill hash.
  foreach my $title (keys %snippets) {
    my $snippet = $snippets{$title};
    my $filename = $snippet->[4];

    next if not defined $snippet->[5]{$DOC_CATEGORY};

    foreach my $category (keys %{$snippet->[5]}) {
      # Omit entry for the 'Included in the official documentation' and
      # 'Snippet' categories; we don't want snippet list files for
      # them.
      next if $category eq $DOC_CATEGORY
                or $category eq $SNIPPET_CATEGORY;

      $snippet_lists{$category}{$filename} = 1;
    }
  }

  print "Creating snippet list files\n";
  for my $key (sort keys %snippet_lists) {
    my $snippet_list = $snippet_lists{$key};

    # Skip empty snippet lists.
    next unless %{$snippet_list};

    my $f = title_to_filename($key =~ s/^Category://r)
            . ".snippet-list";
    my $filename = catfile($snippet_dir, $f);
    print "  $f\n" if $verbose;
    open(my $fh, ">:encoding(UTF-8)", $filename)
      or die "error: Can't open file '$filename': $!\n";
    print $fh map { $_ . "\n" }
                sort filename_sort keys %{$snippet_list};
    close($fh) or warn "warning: Can't close file '$filename': $!";
  }
}


exit 0 if ($no_lsr && $no_new);


# Function:
#   Auxiliary function to convert example blocks to valid Texinfo
#   syntax.
#
# Arguments:
#   $s    String to be manipulated.
#
# Return:
#   The manipulated string.
#
sub postprocess_code_block {
  my ($s) = @_;

  $s =~ s/ \@code\{ (.*?) \} /$1/xmg;
  $s =~ s/\@ / /g;
  $s =~ s/\@\*//g;

  return '@example' . "\n"
         . $s . "\n"
         . '@end example';
}


# Function:
#   Convert MediaWiki data into the Texinfo format.
#
#   We pre- and post-process the MediaWiki data sent to and received
#   from the `pandoc` converter, respectively.
#
#   Pre:
#     * Convert `<samp>...</samp>` to `~samp~...~/samp~` since `pandoc`
#       would swallow these tags unprocessed otherwise (see
#       https://github.com/jgm/pandoc/issues/11299); note that
#       `pandoc`'s `raw_html` extension has no effect since the Texinfo
#       writer ignores it).
#     * Ditto for `<var>...</var>`.
#     * Convert `&nbsp;` and U+00A0 to `~~::~~`.  Pandoc transforms a
#       non-breakable space to `@ `; however, for our purposes `@tie{}`
#       is better.  To avoid overlong output lines we do this temporary
#       replacement before calling pandoc.
#     * Remove Wiki links.  TODO: Shall we support links to doc
#       snippets with `@xref` to the 'Snippets' manual?
#     * Due to a bug (see https://github.com/jgm/pandoc/issues/11312)
#       we have to handle code blocks with inline formatting by
#       ourselves: we surround a series of lines starting with a space
#       with `~example\n\n` and `\n\n~end example`, respectively.  The
#       technique we use to skip `<pre>` blocks is described in
#       https://stackoverflow.com/a/23589204/1276195
#
#   Post:
#     * Remove unusable Texinfo node references to 'Top'.
#     * Escape double quotation marks with a backslash since everything
#       has to be emitted as LilyPond strings.  This can't be done
#       easily before calling pandoc (namely, to avoid too long Texinfo
#       source code lines) because `"` can occur within HTML tags.
#     * Convert `~samp~...~/samp~` to `@samp{...}`.
#     * Convert `~var~...~/var~` to `@var{...}`.
#     * Convert `~~::~~` to `@tie{}`.
#     * Replace `‘...'` (i.e., the single-quote U+2018 and the
#       apostrophe character) with `@q{...}` – it is a historical
#       artifact that Pandoc maps U+2019, the other single-quote
#       character, to the apostrophe (see
#       https://github.com/jgm/pandoc/issues/11316).  For future
#       compatibility, also handle U+2019.
#     * Replace `“...”` (i.e., the double-quote characters U+201C and
#       U+201D) with `@qq{...}`.
#     * Replace `~example\n\n` and `\n\n~end example` with `@example\n`
#       and `\n@end example`, respectively; for the code block
#       inbetween, remove all `@code{...}` and `@*` decorations and
#       replace `@ ` with a plain space.
#
#   Note that we don't check whether there is a paragraph boundary
#   between an opening and a closing HTML tag (ditto for `‘...’` and
#   `“...”`).
#
# Arguments:
#   $s    String to be manipulated.
#
# Return:
#   The manipulated string.
#
sub wiki_to_texinfo {
  my ($s) = @_;

  $s =~ s|<samp\h*>(.*?)</samp\h*>|~samp~$1~/samp~|sg;
  $s =~ s|<var\h*>(.*?)</var\h*>|~var~$1~/var~|sg;
  $s =~ s/(?:&nbsp;|\N{U+00A0})/~~::~~/g;
  $s =~ s/\[\[(.*?)\]\]/$1/sg;

  $s =~ s/
             (?: <pre> (?s) .*? <\/pre> )
             (*SKIP)
             (*FAIL)
           |
             ( (?: ^ [ ] .* \n )+ )
         /
           "~example\n\n" . $1 . "\n~end example\n"
         /xmge;

  $s = pandoc->convert("mediawiki" => "texinfo", $s, "--columns=71");

  $s =~ s/\@node Top\n\@top Top\n\n//;
  $s =~ s|~samp~(.*?)~/samp~|\@samp{$1}|sg;
  $s =~ s|~var~(.*?)~/var~|\@var{$1}|sg;
  $s =~ s/"/\\"/g;
  $s =~ s/~~::~~/\@tie{}/g;
  $s =~ s/\N{U+2018}(.*?)[\N{U+2019}']/\@q{$1}/sg;
  $s =~ s/\N{U+201C}(.*?)\N{U+201D}/\@qq{$1}/sg;

  $s =~ s/
           ^ ~example \n
           \n
           ( (?s) .*? ) \n
           \n ~end [ ] example
         /
           postprocess_code_block($1)
         /xmge;

  return $s;
}


# Function:
#   Call `convert-ly` to update LilyPond code to current syntax.
#
# Arguments:
#   $in    Input sent to `convert-ly`.
#
# Return:
#   A two-element list holding the output of `convert-ly` (stdout and
#   stderr).
#
my $loglevel = $verbose ? "PROGRESS" : "ERROR";

sub convert_ly {
  my ($in) = @_;
  my $out;
  my @err;
  my @args;

  push @args, $CONVERT_LY;
  push @args, "--loglevel=$loglevel";
  push @args, "--diff-version-update" if $diff_version_update;
  push @args, "-";

  eval {
    run3 \@args, \$in, \$out, \@err,
      {binmode_stdin => ":encoding(UTF-8)",
       binmode_stdout => ":encoding(UTF-8)"};
  };

  die "error: $@" if $@;
  die "error: '$CONVERT_LY' killed by signal "
      . ($? & 0x7F) . "\n"
    if ($? & 0x7F);
  die "error: '$CONVERT_LY' exited with error code "
      . ($? >> 8) . "\n"
    if ($? >> 8);

  return ($out, @err);
}


# Start and end of the documentation section of a snippet.
use constant DOC_PREAMBLE => <<~'EOT';
  %% DO NOT EDIT this file manually; it was automatically
  %% generated from the LilyPond Wiki
  %% (https://wiki.lilypond.community).
  %%
  %% Make any changes in the Wiki itself, or in
  %% `Documentation/snippets/new/`, then run
  %% `scripts/auxiliar/makelsr.pl`.
  %%
  %% This file is in the public domain.

  \version "2.24.0"

  \header {
  EOT

# This gets evaluated to eventually expand the variables.
use constant DOC_PREAMBLE_NEW => <<~'EOT';
  %% DO NOT EDIT this file manually; it was automatically
  %% generated from `Documentation/snippets/new/`.
  %%
  %% Make any changes in `Documentation/snippets/new/`,
  %% then run `scripts/auxiliar/makelsr.pl --new`.
  %%
  %% This file is in the public domain.
  %%
  %% Note: this file needs at least LilyPond version $version.

  \version "$version"

  \header {
  EOT

use constant DOC_POSTAMBLE => <<~'EOT';
  } % begin verbatim


  EOT

# For formatting the 'category' header field.
$Text::Wrap::columns = 72;
$Text::Wrap::unexpand = 0;

my $category_prefix = 'categories = "';
my $first_indent = "  ";
my $subsequent_indent = $first_indent
                        . " " x length $category_prefix;


# Emit all snippets from the 'docs' category as files.  This gets
# processed in parallel.
my $pm = Parallel::ForkManager->new($jobs);
$pm->set_waitpid_blocking_sleep(0);

print "Creating snippet files\n";
FILES:
for my $title (sort filename_sort2 keys %snippets) {
  $pm->start and next FILES;

  my $snippet = $snippets{$title};

  if (defined $snippet->[5]{$DOC_CATEGORY}) {
    # New snippets don't have a `<page>` field.
    my $is_new_snippet = !defined $snippet->[0];

    if (($no_lsr && $is_new_snippet) || !$no_lsr) {
      my $f = $snippet->[4];
      my $filename = catfile($snippet_dir, $f);
      my @categories = map { s/^Category://r }
                         grep { $_ ne $DOC_CATEGORY
                                and $_ ne $SNIPPET_CATEGORY }
                           sort keys %{$snippet->[5]};
      my $data;

      if ($is_new_snippet) {
        my $version = $snippet->[1];
        my $s = DOC_PREAMBLE_NEW;
        # Replace variables with itself, but interpolated.
        $s =~ s/(\$\w+)/$1/gee;
        $data .= $s;
      }
      else {
        $data .= DOC_PREAMBLE;
      }

      my $category_string = $category_prefix
                            . join(", ", @categories) . '"' . "\n";
      $data .= wrap($first_indent, $subsequent_indent,
                    $category_string) . "\n";

      my $texidoc = normalize_text($snippet->[2], !$is_new_snippet);
      $texidoc = wiki_to_texinfo($texidoc) if not $is_new_snippet;

      $data .=   '  texidoc = "' . "\n"
               . $texidoc . "\n"
               . '"' . "\n"
               . "\n";

      # The title gets stored as a LilyPond string, too.
      my $doctitle = normalize_text($title, 1);
      $doctitle = wiki_to_texinfo($doctitle) if not $is_new_snippet;

      # No newlines in title.
      $doctitle =~ s/\n+$//;
      $doctitle =~ s/\n/ /g;

      $data .= '  doctitle = "' . $doctitle . '"' . "\n";
      $data .= DOC_POSTAMBLE;

      # Finally, emit the LilyPond snippet code itself.
      my $code = normalize_text($snippet->[3], 0);

      # We have to remove '%LSR' comments, which are not intended to
      # be exported from the LSR.
      $code .= "\n";
      # `%LSR` on a line of its own.
      $code =~ s/^ \h* %+ \h* LSR .* \n//gmx;
      # Trailing `%LSR`.
      $code =~ s/\h* %+ \h* LSR .* $//gmx;

      $data .= $code;

      my @diag = ();
      ($data, @diag) = convert_ly($data) unless $no_convert;

      if (@diag) {
        # Emitting the file name and the diagnose messages (with some
        # indentation) in one call ensures correct log output even if
        # processing in parallel.
        my $d =   "  $filename:\n"
                . "    " . join("    ", @diag);
        $d =~ s/^ \h+ $//gxm;
        print $d;
      }
      else {
        print "  $f\n" if $verbose;
      }

      open(my $fh, ">:encoding(UTF-8)", $filename)
        or die "error: Can't open file '$filename': $!\n";
      print $fh $data;
      close($fh) or warn "warning: Can't close file '$filename': $!";
    }
  }

  $pm->finish;
}

$pm->wait_all_children;

exit 0;


# Local variables:
# perl-indent-level: 2
# indent-tabs-mode: nil
# End:
