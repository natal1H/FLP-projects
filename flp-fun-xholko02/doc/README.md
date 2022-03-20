# FLP 2021/2022 – funkcionální projekt: Haskell

## SIMPLIFY-BKG

- **Autor:** Natália Holková
- **Login:** xholko02

### Popis úlohy

Úlohou je vytvoriť program, ktorý odstráni zbytočné symboly z bezkontextovej gramatiky. Je potrebné postupovať podľa algoritmu 4.3 z opory predmetu TIN.

### Štruktúra priečinku

```
flp-fun-xholko02
│   Makefile - kompilácia
│
└───doc
│   │   README.md - dokumentácia
│   │   test-description.txt - popis priložených testov
│   
└───src
|   │   Main.hs - hlavný program
|   │   ParseInput.hs - načítanie argumentov a zvolenie akcie
|   |   Types.hs - definovanie vlastných typov
|   │   Simplify.hs - algoritmus na odstránenie zbytočných symbolov
|
|___test
    |   test0*.in - vstupný súbor s BKG na testovanie
    |   test0*-1.out - správny výstup testu pri argumente -1
    |   test0*-2.out - správny výstup testu pri argumente -2
```

### Kompilácia

Program je možné skompilovať pomocou nástroja `make` príkazom `make` alebo `make flp21-fun`. Príkaz `make clean` slúži na čistenie po kompilácii.

### Spustenie

Program je možné spustiť príkazom:
```
./flp21-fun (-i|-1|-2) [file]
```

V prípade, že nie je zadaný vstupný súbor bude sa načítať BKG ručne zo vstupu. Je očakávaný rovnaký formát BKG aký by bol v súbore.