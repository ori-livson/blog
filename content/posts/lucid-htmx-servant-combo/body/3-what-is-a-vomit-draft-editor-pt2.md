The flow is:

1. Every second you don't type in the box, the timer **ticks** down.
2. Whenever the text box is edited, the timer **resets** to its starting value.
3. If the timer hits 0:

    - If there is text in the box, we **close the text box and a new editable text box appears below it**.
    - Otherwise, **"time's up!**" All the text you've written so far is merged into a single read-only box to take away for editing.

4. There's also a **"Finalise"** button which stops the timer and merges all the blocks too.

I've put certain phrases in bold because they represent the core API endpoints we will implement.

See [Footnote 2](#footnote-2) some other additions that may improve the editor (and make it harder to work around).