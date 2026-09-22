The interviews that comes to mind are two that he did for python (a less common topic on his channel, but one which I know best).

In the [first one](https://www.youtube.com/watch?v=G6GjnVM_3yM) (that went poorly), the questions start with:

1. How do you check your python version (in particular, how to do it programmatically)?
2. What is a REPL?
3. What is type hinting?
4. What is integer division?
5. Where in memory (i.e., heap vs stack) is a class stored?
6. How do you create an empty class?
7. What is a difference between a tuple and a list?

To reiterate, the point is not to get them all right, but a good candidate should know most of the answers.

Indeed, while the reasons they may not know an answer will may be a bit random, there are some very useful signals in those questions (more so for beginner candidates) -- in particular, answers to:

- Questions 3 and 6 signal that the candidate has looked into other people's code (esp. documentation and open source), where type hinting and empty (abstract base) classes are common, i.e., as opposed to LLM code outputs. Similarly, guides using REPLs are more common in hand-written documentation than LLM outputs.
- Question 1 signal that the candidate does not just run python on perfectly pre-calibrated google colab servers. Tougher variants about dependency versions and packaging would imply that the candidate has gotten their hands dirty due to the inevitable dependency hell python developers eventually reach.
- Similarly, an answer to Question 7 (usually: "tuples are immutable") is usually learned by playing around with tuples and causing an exception to be raised.
- Question 5 (e.g., heap memory for the class, but stack memory for the reference) are rarely explicitly taught in python courses, but being able to think it through implies good fundamentals in operating systems.

In the [second one](https://www.youtube.com/watch?v=hUMTSmYorCM) (that went well), you will find a tougher variety of questions concerning object ids, iterators, the GIL, etc. You can just tell the candidate is bright by the way he answers those questions, I'm not so sure his brightness would have come through by just being asked to invert a binary tree live.
