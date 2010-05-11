Aufgabe 1,2
~~~~~~~~~~~
    siehe vorherige zettel (wir waren wohl schon zu fleissig)

Aufgabe 3
~~~~~~~~~

> data Logic = B Bool | Not Logic | Logic :&: Logic | Logic :|: Logic
> infix 5 :|:
> infix 4 :&:

a) Operationelle Semantik auf der BC-Machine

  (i) Zustandsraum besteht aus Kellerspeicher und Kontrolle 
      auf der Ausdrucke oder die Symbole CNot, CAnd und COr liegen koenen

> data Control = C Logic | CNot | CAnd | COr
> type Machine = ([Bool],[Control])

 (ii) Zu begin ist der Kellerspeicher leer und auf Kontrolle liegt nur der Ausdruck selbst

> begin l = ([], l)

(iii) Die Uebergangsfunktion folgt als Haskell Code 

> delta :: Machine -> Machine
> delta (s, C (B b):c) = (b:s, c)
> 
> delta (s, C (Not l):c) = (s, C l:CNot:c)
> delta (b:s, CNot:c) = ((not b):s, c)
> 
> delta (s, C (l1 :&: l2):c) = (s, C l1:C l2:CAnd:c)
> delta (b1:b2:s, CAnd:c) = ((b1 && b2):s, c)
> 
> delta (s, C (l1 :|: l2):c) = (s, C l1:C l2:COr:c)
> delta (b1:b2:s, COr:c) = ((b1 || b2):s, c)

Die Semantik kann dabei wieder ueber den ausfurhung der delta funktion
deffiniert werden.  In Haskell eignet sich als schlusspunkt fuer die
ausfuehrung der leere Kontroll-Keller:

> o l = run ([], [C l])
>   where run ([s], []) = s
>         run m = run $ delta m

b) Reduktionssemantik

da die sprache kein konzept von zustand hat, koennenen wir logischen
Ausdurcken direkt einen Wert zuweisen

> reduce :: Logic -> Bool

> reduce (B b) = b 
> reduce (Not l) = not b 
>     where b = reduce l
> reduce (l1 :&: l2) = b1 && b2
>     where b1 = reduce l1
>           b2 = reduce l2
> reduce (l1 :|: l2) = b1 || b2
>     where b1 = reduce l1
>           b2 = reduce l2

Die semantik ist dabei wie folgt deffiniert

> eval la = s
>   where s = reduce la

c)


Aufgabe 4
~~~~~~~~~

> example = Not (L True) :|: L True :&: L False
> -- eval example
> -- o example


