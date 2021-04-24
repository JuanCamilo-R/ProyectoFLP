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
     (string ("$"  (arbno (not #\$) ) "$" ) string)
     (character ("'" any whitespace) string) 
     (boolean ( "#"(or "true" "false") ) string)
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
  '((programa (global "endGlobal" expression) a-program)
    (global ( "{" (separated-list  identifier "=" expression  ",") "}" ) exp-global)
    (expression ( "var" "{" (separated-list identifier "=" expression ",") "}"  ";" expression) var-exp)
    (expression ( "const" "{" (separated-list identifier "=" expression ",") "}" ";" expression) const-exp)
    (expression ( "rec" "{" (separated-list identifier "(" (separated-list identifier ",") ")" "=" expression ",") "}" ";" expression) rec-exp)
    (expression ( "unic" "{" (separated-list identifier "=" expression ",") "}" ";" expression) unic-exp)
    (expression (identifier) identifier-exp)
    (expression (number) number-exp)
    (expression (character) character-exp)
    (expression (string) string-exp)
    (expression (boolean) boolean-exp)
    (expression ("allot" identifier "<-" expression) allot-exp)  
    (expression ( "x8" "(" number (arbno "," number ) ")" ) octal-exp) 
    (expression ( "[" (separated-list expression ",") "]" ) list-exp)
    (expression ( "cons" "(" expression  expression ")" ) cons-exp)
    (expression ("func" "(" (separated-list identifier "," ) ")" "{" expression "}") func-exp)
    (expression ("map" "(" expression expression ")") map-exp)
    (expression ("filter" "(" expression expression ")" ) filter-exp)
    (expression ("maxVal" "(" expression ")" ) maxVal-exp) ;list or vector
    (expression ("minVal" "(" expression ")" ) minVal-exp) ;list or vector
    (expression ("n-root" "(" number "," number ")" ) n-root-exp) ;nth-root first argument number base and second one root. 
    (expression ("power-nth" "(" number "," number ")" ) power-exp) ;first argument number base and second one is the power.
    (expression ( "insert-element" "(" number expression expression ")" ) insert-exp) ;list or vector
    (expression ("existsIn" "(" expression expression ")" ) exists-exp) ;list or vector : returns index if exists, -1 otherwise.
    (expression ("remove-element" "(" number expression ")" ) remove-exp) ;list or vector.
    (expression ( "vector" "[" (separated-list expression "," ) "]" ) vector-exp)
    (expression ("record" "{" identifier "=" expression  (arbno "," identifier "=" expression)  "}") record-exp)
    (expression (expr-bool) bool-exp)
    (expr-bool ( "compare"  pred-prim "(" expression "," expression ")") compare-bool-exp) ;(> expression expression)
    (expr-bool ( oper-bin-bool "(" expr-bool "," expr-bool (arbno "," expr-bool) ")" ) oper-bin-bool-exp)
    (expr-bool (primitive-pred-vector "(" expression ")" ) vector-pred-bool-exp) ;vector?
    (expr-bool (primitive-pred-list "(" expression ")" ) list-pred-bool-exp) ;list?
    (expr-bool (primitive-pred-record "(" expression ")" )  record-pred-bool-exp) ;record?
    (expr-bool (primitive-null "(" expression ")" ) null-pred-bool-exp ) ; null list?, null record?, null vector?  
    (expr-bool ("{" boolean "}") simple-bool-exp)
    (expr-bool (oper-a-bool "(" expression ")" ) oper-a-exp)
    (expression ( "sequence" "(" expression (arbno ";" expression) ")") sequence-exp)
    (expression ( "if" "(" expr-bool ")" "then" "{" expression "}" "else"  "{" expression "}") if-exp)
    (expression ( "cond" (arbno "[" expr-bool "]" expression ) "else" expression ) cond-exp)
    (expression ( "for" "(" identifier "=" expression ";" decr-incre-exp expression ")" "{" expression "}") for-exp)
    (decr-incre-exp ("to") to-exp)
    (decr-incre-exp ("downto") downto-exp)
    ;numbers' or octals' expressions.
    (expression ("(" expression primitive expression ")" ) primapp-bin-exp) ;arimetic primitives -> infixed
    (expression (primitive-un "(" expression ")" ) primapp-un-exp)
    ;strings' expressions.
    (expression (primitive-bin-string "(" expression "," expression ")") primapp-bin-string) ;concat  
    (expression (primitive-un-string "(" expression ")" ) primapp-un-string-exp) ;length
    ;lists' expressions.
    (expression (primitive-un-list "(" expression ")") primapp-un-list-exp) ; top, pop, null?
    (expression (primitive-un-create-list "[" (separated-list expression "," ) "]") primapp-create-list) ;create-list
    (expression (primitive-bin-list "(" expression "," expression ")") primapp-bin-list-exp) ;append
    ;vectors' expressions.
    (expression (primitive-bin-set-vector "(" number expression expression ")") primapp-bin-set-vector) ;set-vector
    (expression (primitive-bin-ref-vector "(" number  expression ")") primapp-bin-ref-vector) ;ref-vector 
    (expression (primitive-create-vector "(" (separated-list expression ",") ")" ) primapp-create-vector) ;create vector.

    ;records' expressions.
    (expression (primitive-bin-ref-record "(" number  expression ")" ) primapp-bin-ref-record) ;ref-record.
    (expression (primitive-bin-set-record "(" number  expression expression ")" ) primapp-bin-set-record) ;set-record.
    (expression (primitive-un-create-record "{" identifier "=" expression (arbno "," identifier "=" expression) "}") primapp-create-record) 
    ;arimetic primitives are infixed. 
    (primitive ("+") add-prim)
    (primitive ("-") sub-prim)
    (primitive ("*") mult-prim)
    (primitive ("%") mod-prim)
    (primitive ("/") div-prim)
    (primitive-un ("++") add1-prim)
    (primitive-un ("--") sub1-prim)
    (primitive-un-string ("length") length-prim)
    (primitive-bin-string ("concat") concat-prim)
    ;lists
    (primitive-un-list ("top") head-list-prim)
    (primitive-un-list ("pop") tail-list-prim)
    (primitive-pred-list ("list?") pred-islist-prim)
    (primitive-bin-list ("append") append-list-prim)
    (primitive-un-create-list ("create-list") create-list-prim)
    ;vectors
    (primitive-bin-ref-vector ("ref-vector") ref-vector-prim)
    (primitive-bin-set-vector ("set-vector") set-vector-prim)
    (primitive-pred-vector ("vector?") pred-isvector-prim)
    (primitive-create-vector ("create-vector") create-vector-prim)
     ;records
    (primitive-pred-record ("record?") pred-isrecord-prim)
    (primitive-un-create-record ("create-record") record-create-prim)
    (primitive-bin-ref-record ("ref-record") ref-record-prim)
    (primitive-bin-set-record ("set-record") set-record-prim)
    ;logical expressions
    (pred-prim ("<") less-prim)
    (pred-prim (">") greater-prim)
    (pred-prim ("<=") less-equal-prim)
    (pred-prim (">=") greater-equal-prim)
    (pred-prim ("==") equal-prim)
    (pred-prim ("<>") different-prim)
    (oper-bin-bool ("and") and-prim)
    (oper-bin-bool ("or") or-prim)
    (oper-bin-bool ("xor") xor-prim)
    (oper-a-bool ("not") not-prim)
    (oper-a-bool ("even?") even-bol-prim)
    (oper-a-bool ("odd?") odd-bol-prim)
    ;null-prim
    (primitive-null ("null?") pred-null-prim)
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

