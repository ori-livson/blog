How might we transcend the pitfalls of the Nash Equilibrium of the Prisoner's Dilemma? Douglas Hofstadter (of the famous Gödel Escher Bach) envisions an argument called **super-rationality**, which goes something like:

> "I prefer mutual cooperation to mutual defection, and I understands my opponent agrees. Hence, if I commit to cooperating under this understanding, the fact they are like me means they will do - so we must (and shall) cooperate."

(See: [Metamagical Themes](https://en.wikipedia.org/wiki/Metamagical_Themas) for more information)

To formalise this argument, we need to formalise the self-referential definition of the other player being "like me".

Barasz, Christiano, Fallenstein, Herreshoff, LaVictoire and Yudkowsky recently explored this question in several works stemming from [their 2014 paper](https://arxiv.org/abs/1401.5577) that consider important possibilities regarding algorithms caught in a Prisoner's Dilemma - but with the twist that the algorithms can read each other's source code before making a choice.

In this setting, one could in theory design an algorithm cooperates in certain circumstances, but would not be so naive as to cooperate with an opponent that can be proven to always defect.

Moreover, Kleene's Fixed-Point Theorem (or equivalently, the Diagonalisastion Lemma<sup>[5](#footnote-5)</sup>) allows us to demonstrate the existence of algorithms that are super-rational in the sense that they will specifically cooperate with other algorithms that are super-rational in the same manner.
