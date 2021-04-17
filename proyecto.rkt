#lang eopl

(define lexica
  '( (white-sp
      (whitespace) skip)
     (comment
      ("//" (arbno (not #\newline))) skip)
     (comment
      ("/*" (arbno any) "*/") skip)
     (identifier
      (letter (arbno (or letter digit "_" "-" "?"))) symbol)
     (string ( "$" letter (arbno (or letter digit whitespace)) "$" ) string)
     (character ("'" any whitespace) string) 
     (bolean ( "#"(or "verdadero" "falso") ) string)
     (number
      (digit (arbno digit)) number)
     (number
      ("-" digit (arbno digit)) number)
     (number
      (digit (arbno digit) "." digit (arbno digit)) number)
     (number
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
  '((programa (global "endGlobal" expression) un-programa)
    (global ( "{" (separated-list  identifier "=" expression  ",") "}" ) exp-global)
    (expression ( "var" "{" (separated-list identifier "=" expression ",") "}"  ";" expression) var-exp)
    (expression ( "const" "{" (separated-list identifier "=" expression ",") "}" ";" expression) cons-exp)
    (expression ( "rec" "{" (separated-list identifier (separated-list identifier ",") "=" expression ",") "}" ";" expression) rec-exp)
    (expression ( "unic" "{" (separated-list identifier "=" expression ",") "}" ";" expression) unic-exp)
    (expression (number) numero-exp)
    (expression (character) caracter-exp)
    (expression (string) cadena-exp)
    (expression (bolean) bolean-exp)
    (expression ( "[" (separated-list expression ";") "]" ) lista-exp)
    (expression ( "vector" "[" (separated-list expression ";" ) "]" ) vector-exp)
    (expression ("record" "{" identifier "=" expression  (arbno "," identifier "=" expression)  "}") registro*-exp)
    (expression (expr-bool) bool-exp)
    (expr-bool ( "compare"  pred-prim "(" expression "," expression ")") compare-exp)
    (expr-bool ( oper-bin-bool "(" expr-bool "," expr-bool ")" ) oper-bin-exp)
    (expr-bool ("{" bolean "}") simpe-bool-exp)
    (expr-bool (oper-un-bool "(" expr-bool ")" ) oper-un-exp)
    (expression ( "sequence" "(" expression (arbno ";" expression) ")") sequence-exp)
    (expression ( "if" "(" expr-bool ")" "then" "{" expression "}" "else"  "{" expression "}") if-exp)
    (expression ( "cond" (arbno "[" expression "]" expression ) "else" expression ) cond-exp)
    (expression ( "for" "(" identifier "=" expression ";" decr-incre-exp expression ")" "{" expression "}") for-exp)
    (decr-incre-exp ("to") to-exp)
    (decr-incre-exp ("downto") downto-exp)
    (expression (primitive "(" (separated-list expression "," ) ")" ) primapp-exp)
    
    (primitive ("+") add-prim)
    (primitive ("-") sub-prim)
    (primitive ("*") mult-prim)
    (primitive ("%") mod-prim)
    (primitive ("/") div-prim)
    (primitive ("++") add1-prim)
    (primitive ("--") sub1-prim)
    (pred-prim ("<") less)
    (pred-prim (">") greater)
    (pred-prim ("<=") less-equal)
    (pred-prim (">=") greater-equal)
    (pred-prim ("==") equal)
    (pred-prim ("<>") different)
    (oper-bin-bool ("and") and)
    (oper-bin-bool ("or") or)
    (oper-bin-bool ("xor") xor)
    (oper-un-bool ("not") not)
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

