Achtung: Das Projekt steckt noch in den Anfängen.

ABAP ist eine Programmiersprache, die historisch gewachsen ist. D.h. man hat neue Sprachelemente hinzugefügt, aber aus Kompatibilitätsgründen die alte Syntax nicht geändert.
Das könnte ändern, indem man den Code parst und mit moderner Syntax wieder ausgibt. Da es sich um reine Syntax-Änderungen handelt, kann es auch keine bösen Überraschungen geben.
Ziel dieses Code ist ein Eclipse-Plugin zu entwickeln, das einen Parser verwendet, um alte Syntax in moderne Syntax und umgekehrt zu verwandeln. Ähnlich zu der Idee von Typescript nach Javascript-Transpilierung.

Beispiel Case in ABAP:

```ABAP
CASE <variable>.
  WHEN <value1>.
    " Anweisungen
  WHEN <value2> OR <value3>.
    " Anweisungen
  WHEN OTHERS.
    " Anweisungen
ENDCASE.
```

Case in ABAP SQL (Man hat hier die Punkte weggelassen, WHEN OTHERS durch ELSE ersetzt und ENDCASE durch END AS)

```ABAP
CASE 
  WHEN <bedingung1> THEN <wert1>
  WHEN <bedingung2> THEN <wert2>
  ELSE <wert>
END AS <alias>
```

Ziel dieses Projekts ist es
* den Code zu parsen in einen AST
* BEGIN-END-Pascal-Syntax durch geschweifte Klammern zu ersetzen, Punkt durch Semikolon, usw., eine Java-ähnliche, einheitliche Syntax.
* ein Eclipse-Plugin zu entwickeln, dass diesen Parser als .exe ausführt
* Parser für moderne Syntax in alte Syntax

** Benutzung
Haskell hat sehr elegante, schnelle Parser-Bibliotheken. Deshalb wurde Megaparsec verwendet.
Bauen mit
```bash
cabal build
```
