The flow is:

1. Every second you don't type in the box, the timer **ticks** down.
2. Whenever edit the text in the box, the timer **resets** to its starting value.
3. If the timer hits 0:
    - If there is text in the box, we **close the box and a new box appears below it** for you to continue typing.
    - Otherwise, **"time's up!**" All the text you've written so far is merged into a single read-only box to take for editing (or to try a vomit draft).

4. There's also a **"Finalise"** button which stops the timer and merges all the blocks too.

I've put certain phrases in bold because they are the core API endpoints we will implement.

See [Footnote 2](#footnote-2) some other additions that may improve the editor (and make it harder to work around).