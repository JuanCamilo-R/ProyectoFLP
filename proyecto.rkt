#lang eopl

(define lexica
  '( (white-sp
      (whitespace) skip)
     (comment
      ("//" (arbno (not #\newline))) skip)
     (comment
      ("/*" (arbno any) "*/") skip)
     (identificador
      (letter (arbno (or letter digit "_" "-" "?"))) symbol)
     (cadena ( "$" letter (arbno (or letter digit whitespace)) "$" ) string)
     (caracter ("'" any whitespace) string) 
     (boleano ( "#"(or "verdadero" "falso") ) string)
     (numero
      (digit (arbno digit)) number)
     (numero
      ("-" digit (arbno digit)) number)
     (numero
      (digit (arbno digit) "." digit (arbno digit)) number)
     (numero
      ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

(define retornarString
  (lambda (str)
    (substring str 1 (-(string-length str)1) )
    )
  )

(define retornarCaracter
  (lambda (char)
    (substring char 1)
    )
  )

(define gramatica
  '((programa (globales "endGlobal" expresion) un-programa)
    (globales ( "{" (separated-list  identificador "=" expresion  ",") "}" ) exp-global)
    (expresion ( "var" "{" (separated-list identificador "=" expresion ",") "}"  ";" expresion) var-exp)
    (expresion ( "const" "{" (separated-list identificador "=" expresion ",") "}" ";" expresion) cons-exp)
    (expresion ( "rec" "{" (separated-list identificador (separated-list identificador ",") "=" expresion ",") "}" ";" expresion) rec-exp)
    (expresion ( "unic" "{" (separated-list identificador "=" expresion ",") "}" ";" expresion) unic-exp)
    (expresion (numero) numero-exp)
    (expresion (caracter) caracter-exp)
    (expresion (cadena) cadena-exp)
    (expresion (boleano) boleano-exp)
    (expresion ( "[" (separated-list expresion ";") "]" ) lista-exp)
    (expresion ( "vector" "[" (separated-list expresion ";" ) "]" ) vector-exp)
    (expresion ("record" "{" identificador "=" expresion  (arbno "," identificador "=" expresion)  "}") registro*-exp)
   )
  )




(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

