Info for Documentation
----------------------

%%%%% GENERAL POLICY

Formatting: writing-texinfo.txt
General policy: policy.txt


%%%%% UPDATING DOCS
cd into Documentation and run

find -name . '*.itely' | xargs convert-ly -e

(This also updates translated docs.)
