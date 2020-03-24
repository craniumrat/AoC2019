I was unsure how to parse the directions, but after watching Michael's video on Day 3, he used a very elegant way to parse the input.

I simply copy-pasted that for my earlier solution.

This time, I will try to solve it using a parser. Fingers's crossed.. Googling parsec parsing in 3..2..1

Found solution by mixing information in:
https://stackoverflow.com/questions/49758970/parsing-a-sum-datatype-with-parsec
https://jakewheat.github.io/intro_to_parsing/#applicative-style-parsing-code

Figured out how to apply parser to string in
https://www.futurelearn.com/courses/functional-programming-haskell/0/steps/27222

Tried to create a list by appending each coordinate calculated by walking the path from each direction. Unfortunately, that eats up all the memory, causing a mmap error.

Round 2: We don't need to keep the path, and there might be plenty of duplicates nodes for each chain, so we just maintain a set of points and the current node we are processing.

Good news, got the correct answer1.

Round 3: While waiting to solve the crash in Round 1, I clicked on Michael Gilliland's video, and he started off by using unfoldr. I paused the video there and I will attempt to make the toCoordsSet using unfoldlr. I had to unpause and watch a lot more of the unfoldr implementation. :)...




