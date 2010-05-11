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

c) Aequivalenzbeweis

Da weder Zustand noch Schleifen vorhanden sind, kann die Aequivalentz ueber
strukturelle Induktion ueber den Ausdrucksbaum bewiesen werden.

Es ist zu zeigen, dass eval l = o l fuer alle Logischen Ausdrucke l.
Es recht aus Folgendes zu zeigen:
    reduce l = b 
 gdw. 
    delta ([], l:c) = delta ([b], c)
Aus der definition von eval bzw. o folgt draus die Aquevalentz der Semantik.

IB: Behauptung gild fuer Bleatter im Syntax-Baum
    einziges Blatt ist (B Bool), und es gilt:
      reduce (B b) = b
      delta([], C (L b):c) = delta([b], c)

IA: fuer Teilbaume eines Knotens im Syntax-Baum gilt:
    delta ([], l:c) -> delta ([b], c)
        mit b = reduce l

IS: Fall 1: (Not l)
      reduce (Not l) = not $ reduce l = not b = b'
      delta ([], Not l:c) 
        -> delta ([], l:CNot:c)
        -> delta ([b], CNot:c) -- nach IA
        -> delta ([not b], c)  -- def dleta
        -> delta ([b'], c)
            mit b' = reduce (Not l)
      qed.

    Fall 2: (l1 :&: l2)
      reduce (l1 :&: l2) = (reduce l1) && (reduce l2) = b1 && b2 = b'
      delta ([], (l1 :&: l2):c) 
        -> delta ([], l1:l2:CAnd:c)
        -> delta ([b1], l2:CAnd:c) -- nach IA
        -> delta ([b1,b2], CAnd:c) -- nach IA
        -> delta ([b1 && b2], c)   -- def dleta
        -> delta ([b'], c)
            mit b' = reduce (l1 :&: l2)
      qed

    Fall 3: analog


Aufgabe 4
~~~~~~~~~

> example = Not (B True) :|: B True :&: B False
> -- eval example
> -- o    example
