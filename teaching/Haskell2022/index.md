---
layout: post
title: "ASQ Funktionale Programmierung"
date:   2022-10-25 10:00:00 +0100
permalink: /teaching/Haskell2022/
---

# Unterlagen

|-|-|-|
| Tag | Slides | Code | Notes |
|-|-|-|-|
| 2022-10-20 | [Slides](01.pdf) | [Haskell](01.hs) | |
| 2022-10-27 | [Slides](02.pdf) | [Haskell](02.hs) | |
| 2022-11-03 | [Slides](03.pdf) | [Haskell](03.hs) | |
| 2022-11-10 | [Slides](04.pdf) | [Haskell](04.hs) | [Video Countdown, Minute 26](https://youtu.be/isyYDxxXMjk?t=1560) |
| 2022-11-17 | [Slides](05.pdf) | [Haskell](05.hs) | |

Bitte beachtet das die Unterlagen nur begleitend zur VL sind, ich erkläre viel an der Tafel und am Code, Selbststudium mit den Folien ist schwer bis unmöglich

## Kategorientheorie und Haskell

Die folgenden Links sind explizit *nicht* Teil der Vorlesung und koennen gerne ignoriert werden. Die
Vorlesung soll primaer eine Einfuehrung in Haskell sein.

[Kategorientheorie](https://de.wikipedia.org/wiki/Kategorientheorie) ist ein modernes Teilgebiet der
Mathematik und dient der Beschreibung mathematischer Strukturen. Eine Begriffe aus der
Kategorientheorie haben Eingang in den Wortschatz von Haskell gefunden.

Wer wirklich Interesse an einem Einstieg in die Kategorientheorie und ihre Verbindung zum
Programmieren hat, kann mit Bartosz Milewski beginnen:
- <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>
- <https://github.com/hmemcpy/milewski-ctfp-pdf>

## "Hausaufgabe" VL 05 -> VL 06

- Versuchen sie zur naechsten VL eine Funktion *showExpr :: Expr -> String* zu schreiben
- Diese sollte einen korrekten Klammerstring erzeugen
- Eingabe zB ``App Add (Num 1) (Num 2)``
- Ausgabe "1+2"
- Wenn man die Ausgaben in ``token2Expr . tokenize`` hinein steckt dann sollte wieder die Eingabe
  dabei heraus kommen
- Das bedeutet das ``id == token2Expr . tokenize . showExpr``, also ``Expr`` die man erst mittels
  ``showExpr`` anzeigt und danach parsed genau wieder die Expression ergeben sollten

# Wo?

- ab *Donnerstag, 2022-10-27* findet die Vorlesung hier statt:
- Jentower, Leutragraben 1, 07743 Jena
- Raum: 08N04

- den Jentower betreten
- die Fahrstühle finden, sie befinden sich neben der Rezeption im Erdgeschoss
- vor den Fahrstühlen die "8" drücken
- ein Fahrstuhl A--F wird angezeigt, diesen betreten und im 8. Stock aussteigen
- dort gibt es zwei Glastüren, eine davon werde ich euch auf lassen
- den Raum 08N04 finden (nach der Glastür gleich links am Gang)
