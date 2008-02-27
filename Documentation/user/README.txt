Info for Documentation
----------------------

%%%%% GENERAL POLICY

Formatting: writing-texinfo.txt
General policy: policy.txt


%%%%% UPDATING DOCS
convert-ly -e --from=... --to=... --no-version *.itely

% to find the current version number,
grep "version \"" fundamental.itely

Please don't forget updating translated docs (in LANGS/user) too!

%  (nobody ever remembers to update this file, so I've stopped
%  trying to record it here)


