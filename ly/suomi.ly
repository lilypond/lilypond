% common finnish names for notes. "es" means flat, "is" means sharp
%
% by Heikki Junes <heikki.junes@hut.fi>
% based on svenska.ly by Mats Bengtsson.

% 2001/08/28 Heikki Junes <heikki.junes@hut.fi>
%            added bb and marked heses as `unusual'

\pitchnames #`(
	(ceses . ,(ly:make-pitch -1 0 -2 ))
	(ces . ,(ly:make-pitch -1 0 -1 ))
	(c . ,(ly:make-pitch -1 0 0 ))
	(cis . ,(ly:make-pitch -1 0 1 ))
	(cisis . ,(ly:make-pitch -1 0 2 ))
	(deses . ,(ly:make-pitch -1 1 -2 ))
	(des . ,(ly:make-pitch -1 1 -1 ))
	(d . ,(ly:make-pitch -1 1 0 ))
	(dis . ,(ly:make-pitch -1 1 1 ))
	(disis . ,(ly:make-pitch -1 1 2 ))
	(eses . ,(ly:make-pitch -1 2 -2 ))
	(es . ,(ly:make-pitch -1 2 -1 ))
	(e . ,(ly:make-pitch -1 2 0 ))
	(eis . ,(ly:make-pitch -1 2 1 ))
	(eisis . ,(ly:make-pitch -1 2 2 ))
	(feses . ,(ly:make-pitch -1 3 -2 ))
	(fes . ,(ly:make-pitch -1 3 -1 ))
	(f . ,(ly:make-pitch -1 3 0 ))
	(fis . ,(ly:make-pitch -1 3 1 ))
	(fisis . ,(ly:make-pitch -1 3 2 ))
	(geses . ,(ly:make-pitch -1 4 -2 ))
	(ges . ,(ly:make-pitch -1 4 -1 ))
	(g . ,(ly:make-pitch -1 4 0 ))
	(gis . ,(ly:make-pitch -1 4 1 ))
	(gisis . ,(ly:make-pitch -1 4 2 ))
	(asas . ,(ly:make-pitch -1 5 -2 ))
	(ases . ,(ly:make-pitch -1 5 -2 ))   ;;non-standard name for asas
	(as . ,(ly:make-pitch -1 5 -1 ))
	(a . ,(ly:make-pitch -1 5 0 ))
	(ais . ,(ly:make-pitch -1 5 1 ))
	(aisis . ,(ly:make-pitch -1 5 2 ))
	(bb . ,(ly:make-pitch -1 6 -2 ))
	(heses . ,(ly:make-pitch -1 6 -2 ))  ;;non-standard name for bb
	(b . ,(ly:make-pitch -1 6 -1 ))
	(h . ,(ly:make-pitch -1 6 0 ))
	(his . ,(ly:make-pitch -1 6 1 ))
	(hisis . ,(ly:make-pitch -1 6 2 ))
)
\version "1.7.3"
