#!/bin/bash

# convert a Google code CSV file into a countdown announcement
#
# USAGE:
#   make-countdown-announcement.sh NEXT_DEADLINE_DATE [PATCH-MEISTER]
#     OR
#   make-countdown-announcement.sh NEXT_DEADLINE_DATE [PATCH-MEISTER] > OUTPUT-FILE
#
# EXAMPLES:
#   make-countdown-announcement.sh "Jan 1, 2001" "John Doe"
#   make-countdown-announcement.sh "Jan 1, 2001" John
#   make-countdown-announcement.sh "Jan 1, 2001" > countdown.txt


CSV_FILE=lilypond-issues.csv # comma-separated values
TSV_FILE=lilypond-issues.tsv # tab-separated values
URL_BASE="https://code.google.com/p/lilypond/issues"
QUERY_STR="q=Patch%3Apush%2Ccountdown%2Creview%2Cnew%2Cwaiting&colspec=Patch%20Owner%20ID%20Summary&sort=patch"
DEADLINE=$1
PATCH_MEISTER=${2:-"The Patch Meister"}
MAILMAP='
"adam.spiers","Adam Spiers"
"aleksandr.andreev","Aleksandr Andreev"
"benko.pal","Benkő Pál"
"bordage.bertrand","Bertrand Bordage"
"brownian.box","Dmytro O. Redchuk"
"Carl.D.Sorensen","Carl Sorensen"
"carlopeterson","Carl Peterson"
"colinghall","Colin Hall"
"ColinPKCampbell","Colin Campbell"
"d8valily","Mark Mathias"
"dak@gnu.org","David Kastrup"
"david.nalesnik","David Nalesnik"
"dschudy","Devon Schudy"
"Eluzew","Eluze W"
"erlenda","Erlend Aasland"
"fedelogy","Federico Bruni"
"frederic.bron.1995@polytechnique.org","Frédéric Bron"
"graham@percival-music.ca","Graham Percival"
"hanwenn","Han-Wen Nienhuys"
"hjunes","Heikki Junes"
"ht.lilypond.development","Heikki Tauriainen"
"ianhulin44","Ian Hulin"
"idragosani","Brett McCoy"
"jameselihubailey","James E. Bailey"
"janek.lilypond","Janek Warchoł"
"jan.nieuwenhuizen","Jan Nieuwenhuizen"
"joeneeman","Joe Neeman"
"john.mandereau","John Mandereau"
"joseph.wakeling","Joseph Wakeling"
"julien.rioux","Julien Rioux"
"kieren@alumni.rice.edu","Kieren MacMillan"
"k-ohara5a5a@oco.net","Keith OHara"
"lemzwerg","Werner Lemberg"
"lilyliska","Urs Liska"
"lilypond.patchy.graham","Graham Percival"
"mandolaerik","Erik Sandberg"
"marc@hohlart.de","Marc Hohl"
"marek@gregoriana.sk","Marek Klein"
"markpolesky","Mark Polesky"
"milimetr88","Łukasz Czerwiński"
"mtsolo","Mike Solomon"
"neziap","Janek Warchoł"
"nicolas.sceaux","Nicolas Sceaux"
"n.puttock","Neil Puttock"
"paconet.org","Francisco Vila"
"PhilEHolmes","Phil Holmes"
"pkx166h","James Lowe"
"plroskin","Pavel Roskin"
"pnorcks","Patrick McCarty"
"RalphBugList","Ralph Palmer"
"reinhold.kainhofer","Reinhold Kainhofer"
"rzedeler","Rune Zedeler"
"schilke.60","Derek Klinge"
"shingarov","Boris Shingarov"
"tdanielsmusic","Trevor Daniels"
"thomasmorley65","Thomas Morley"
"v.villenave","Valentin Villenave"
'


usage() {
  cat << EOF >&2
USAGE:
  `basename $0` NEXT_DEADLINE_DATE [PATCH-MEISTER]
    OR
  `basename $0` NEXT_DEADLINE_DATE [PATCH-MEISTER] > OUTPUT-FILE

EXAMPLES:
  `basename $0` "Jan 1, 2001" "John Doe"
  `basename $0` "Jan 1, 2001" John
  `basename $0` "Jan 1, 2001" > countdown.txt
EOF
  exit 1
}


# display USAGE
case $# in
  1|2) case $1 in
         -h|--help) usage ;;
         *) ;;
       esac ;;
  *) usage ;;
esac


# clean up in case of a previous interrupt
remove-if-exists() {
  if [ -e $1 ]; then rm $1; fi
}
remove-if-exists push.tmp
remove-if-exists countdown.tmp
remove-if-exists review.tmp
remove-if-exists new.tmp
remove-if-exists waiting.tmp
remove-if-exists $TSV_FILE


# show initial instructions
read -p "First, in your web browser, make sure you're logged in to:

  $URL_BASE/list

Then save the following link as a file called \"$CSV_FILE\"
(save it to this directory -- $PWD/):

  $URL_BASE/csv?$QUERY_STR

Then press enter"


if [ ! -e $CSV_FILE ]; then
  echo -e "\nError: can't find \`$CSV_FILE': No such file or directory" >&2
  exit 1
fi


# If the user wasn't logged in when downloading $CSV_FILE, the emails will
# appear truncated, e.g. "abc...@gmail.com", and won't be properly replaced by
# the author names later on.  This code block complains if the string "...@" is
# found anywhere in the email field.
if [ "`cut -d, -f2 $CSV_FILE | grep -o '\.\.\.@'`" ]; then
    read -p "
It looks like you were not logged in to Google code when you downloaded
$CSV_FILE.  Continue anyway (y/n)? "
  while :
  do
    case "$REPLY" in
      y) break ;;
      n) exit 1 ;;
      *) read -p "Enter \`y' to continue; \`n' to quit and try again: "
    esac
  done
fi


# change the quoted, comma-separated file
# into an unquoted, tab-separated file
# and keep only the first 4 fields: Patch,Owner,ID,Summary
sed '{
1d
/^$/d
s/\t/ /g
s/^"//
s/","/\t/g
s/",*$//
s/""/"/g
}' $CSV_FILE | cut -sf1-4 > $TSV_FILE


ISSUES_WITH_MISSING_FIELDS=`sed -n '/\t\t\|\t$/p' $TSV_FILE |
                            awk -F"\t" '{ print $3 }'`

if [ "$ISSUES_WITH_MISSING_FIELDS" ]; then
  COUNT=`wc --lines <(echo "$ISSUES_WITH_MISSING_FIELDS") | sed 's/ .*//'`
  LINKS_WITH_MISSING_FIELDS=`echo "$ISSUES_WITH_MISSING_FIELDS" |
                             sed "s!^!  $URL_BASE/detail?id=!"`
  if [ $COUNT -eq 1 ]; then
    echo -e "\nError: The following issue is missing an OWNER or SUMMARY:" >&2
  else
    echo -e "\nError: Each of the following issues is missing an OWNER or SUMMARY:" >&2
  fi
  echo "$LINKS_WITH_MISSING_FIELDS" >&2
  echo "Please add the missing information in the tracker and start over." >&2
  remove-if-exists $TSV_FILE
  exit 1
fi


EMAILS_USED=`awk -F"\t" '{ print $2 }' $TSV_FILE | sort --unique`
KNOWN_EMAILS=`
IFS=$'\n'
for i in $MAILMAP; do
  echo "$i" | sed 's/^"\(.*\)",".*/\1/'
done | sort --unique`
UNKNOWN_EMAILS=`comm -23 <(echo "$EMAILS_USED") <(echo "$KNOWN_EMAILS")`


# Unknown emails/usernames usually mean new contributors.
# If any are found, the user is prompted to add the new
# authors' names to the MAILMAP list above.
if [ "$UNKNOWN_EMAILS" ]; then
  COUNT=`wc --lines <(echo "$UNKNOWN_EMAILS") | sed 's/ .*//'`
  if [ $COUNT -eq 1 ]; then
    echo -e "\nError: The following email/username needs to be associated with a name:" >&2
    echo "$UNKNOWN_EMAILS" | sed 's/^/  /' >&2
    echo -e "Please add it to the MAILMAP list in\n  $0" >&2
  else
    echo -e "\nError: The following emails/usernames need to be associated with names:" >&2
    echo "$UNKNOWN_EMAILS" | sed 's/^/  /' >&2
    echo -e "Please add them to the MAILMAP list in\n  $0" >&2
  fi
  echo "using the following form:" >&2
  echo "$UNKNOWN_EMAILS" | sed 's/.*/  "&","author name"/' >&2
  echo "and commit your changes to the main git repository." >&2
  remove-if-exists $TSV_FILE
  exit 1
fi


EMAIL_REPLACEMENTS=`
IFS=$'\n'
for i in $MAILMAP; do
  echo "$i" | sed 's/^"\(.*\)","\(.*\)"$/s\/\1\/\2\/g/'
done`


# replace emails/usernames with author names
# and separate the issues by patch type
sed -n "{
$EMAIL_REPLACEMENTS
/^push/w push.tmp
/^countdown/w countdown.tmp
/^review/w review.tmp
/^new/w new.tmp
/^waiting/w waiting.tmp
}" $TSV_FILE


# clean up; $TSV_FILE is no longer needed
remove-if-exists $TSV_FILE


format-entry() {
  TYPE=$1
  FILE="$1.tmp"
  if [ -s $FILE ]; then
    echo -e "$TYPE:\n" | tr 'a-z' 'A-Z'
    awk -v UB=$URL_BASE -F"\t" \
      '{ printf("%s: %s\n%s/detail?id=%d\n\n"), $2, $4, UB, $3 }' $FILE
    echo -e "____________________\n\n"
  fi
  remove-if-exists $FILE
}

echo >&2


# display the countdown announcement
echo "Hello,

Here is the current patch countdown list.
The next countdown will be on $DEADLINE.

You can always view the most current countdown list here:
$URL_BASE/list?$QUERY_STR

____________________

"

format-entry push
format-entry countdown
format-entry review
format-entry new
format-entry waiting

echo -e "Thank you,\n$PATCH_MEISTER"
