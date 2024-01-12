---
layout: post
title: "ASQ Funktionale Programmierung"
date:   2023-10-19 10:00:00 +0100
permalink: /teaching/Haskell2023/
---
<!-- LTeX: language=de-DE -->


# Unterlagen

Die Videos zur Vorlesung finden sich jeweils hier:
<https://cloud.uni-jena.de/s/QowbJYK3dZGRaHe>

|------------|-------------------------------------------|----------------------------------------------------------|-------------------------------------------------------------------------------------|
| Tag        | Slides                                    | Code                                                     | Notes                                                                               |
|------------|-------------------------------------------|----------------------------------------------------------|-------------------------------------------------------------------------------------|
| 2023-10-19 | [Slides](01.pdf)                          |                                                          |                                                                                     |
| 2023-10-26 | [Slides](02.pdf)                          | [Code](02.hs)                                            |                                                                                     |
| 2023-11-02 | [Slides](03.pdf)                          | [Code](03.hs)                                            | [List comprehensions](03-comprehension.hs) and [Fibonacci](03-fib.hs)               |
|            |                                           |                                                          | [Hausaufgabe zum Parsing. Beschreibung und Hilfsfunktionen im Code](03/homework.hs) |
| 2023-11-09 | [Token Parsing](token-parsing.pdf)        | [Code - token](tokenparsing.hs)                          |                                                                                     |
| 2023-11-16 | [Monadic Parsing](monadic-parsing.pdf)    | [Code - monadic](monadicparsing.hs)                      | Vorlesungsvideos verfügbar                                                          |
|            |                                           | [Hausaufgaben zum 23.](04/monadicparsing.hs)             | Die Hausaufgaben finden sich ab Zeile 417 im Link links                             |
| 2023-11-23 | [Monadic Parsing](04/monadic-parsing.pdf) | [Code](04/monadicparsing.hs)                             | "Deep dive" in Monaden, Parsing, eure Fragen, und die Lösungen zu den Hausaufgaben  |
| 2023-11-30 | [Rush hour Intro](rushhour/rushhour.pdf)  |                                                          | Einführung zum Spiel Rush hour. Video in der uni jena cloud                         |
| 2023-12-04 | [Rush hour hints](rushhour/hints.pdf)     |                                                          | Hinweise zu Datenstrukturen die man in Rushhour nutzen kann                         |
| 2023-12-04 | [Monad transformer](mtl.pdf)              | [Code](mtl.hs)                                           | Monad transfomer Code und Slides. Video in der Uni Jena Cloud                       |
| 2023-12-04 | [Typfamilien](families/families.pdf)      | [Code](families/families.hs)                             | Typfamilien und ein bischen Typlevel-Programmierung                                 |
| 2023-12-07 |                                           | [Code](Combi.hs)                                         | Minibeispiel zu den Funktionen ``(.)`` und ``($)`` zur Funktionskomposition         |
| 2024-01-03 |                                           | [Rushhour - Vorlesung](rushhour/rushhour-0.1.0.0.tar.gz) | Beginn der Rushhour-Implementation "live" in der Vorlesung                          |
| 2024-01-03 |                                           | [Rushhour - Choener](rushhour/RushChoener.hs)            | Meine Implementation von rushhour                                                   |
| 2024-01-06 | [Rekursion](recursion/09.pdf)             | [Rekursion](recursion/recursions.hs)                     | Video in der Cloud                                                                  |
| 2024-01-18 | [Arrays](arrays/arrays.pdf)               | [Array](arrays/Array.hs)                                 | Kurz-Intro zu Haskell-Arrays, Video in der Cloud                                    |
| 2024-01-18 | [Streams](streams/streams.pdf)            | [streams](streams/streams.hs)                            | Stream fusion, Video in der Cloud                                                   |
| 2024-01-18 | Hausaufgabe ...                           |                                                          | Siehe unten zu "Hausaufgabe Streams"                                                |

## 2024-01-18, Hausaufgabe Streams

Unter [Data.List](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html) finden sich die wichtigsten Funktionen auf Haskell-Listen.

Folgende Funktionen sind interessant: ``(++), length, map, foldl', scanl', unfoldr, lookup, zip``.

1. Versteht was jede einzelne Funktion tut. Basierend auf ihrem Typ, der Beschreibung, und dem jeweiligem Beispiel.
2. Lest *nicht* den Source code, auch nicht den vom "vector" package.
3. Implementiert die entsprechenden Funktionen basierend auf ``streams`` wie es oben zur Verfügung steht.

Hinweis: der Constraint ``HasCallStack`` kann ignoriert werden.


Bitte beachtet, dass die Unterlagen nur begleitend zur VL sind, ich erkläre viel an der Tafel und am Code, Selbststudium mit den Folien ist schwer bis unmöglich

## Lernmaterial für Haskell

- [Learn you a Haskell](http://learnyouahaskell.com/)
  - easy to follow tutorial on programming with Haskell, in english


