Unfortunately, I lost all my files and code because my 2 year old daughter accidentally pressed a key at the wrong time on Chrome OS startup and I lost all my code from Day 1 to Day 16.

This is me a little wiser and more careful and a little more don't-put-off-backups-because-you-know-ey.


The first thing I wanted to do was to figure out how to read integers into a list. The function is straight forward. Applying it to a list is straight forward. For a Haskell beginer, reading a file in Haskell into a list is a hairy proposition. Googling in 3...2..1.

Ok, answer1 works. :) But, I had to ensure that there are no empty lines in the input file, else the code fails with a "read: no parse" on the empty file.

TODO: Make the code more robust about ignoring empty lines.

TODO: We can break up the input into multiple chunks and parallelize the computation. That would be a good introduction to parallelizing Haskell code.
