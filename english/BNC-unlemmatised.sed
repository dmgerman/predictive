#!/usr/bin/sed -f

# get rid of the last two fields and reverse the order of the first two
s/^\(.*\) \(.*\) .* .*/\2 \1/

# delete words with weird characters in
/.*[0123456789~@#$%&*+=\/()<>_:;\"\`,.!?].* .*/ d

# delete words that start or end with an apostrophe or hyphen
/^'/ d
/^-/ d
/' / d
/- / d
