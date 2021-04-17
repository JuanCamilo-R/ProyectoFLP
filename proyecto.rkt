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
  '((programa (global "endGlobal" expression) a-programa)
    (global ( "{" (separated-list  identifier "=" expression  ",") "}" ) exp-global)
    (expression ( "var" "{" (separated-list identifier "=" expression ",") "}"  ";" expression) var-exp)
    (expression ( "const" "{" (separated-list identifier "=" expression ",") "}" ";" expression) cons-exp)
    (expression ( "rec" "{" (separated-list identifier (separated-list identifier ",") "=" expression ",") "}" ";" expression) rec-exp)
    (expression ( "unic" "{" (separated-list identifier "=" expression ",") "}" ";" expression) unic-exp)
    (expression (number) number-exp)
    (expression (character) character-exp)
    (expression (string) string-exp)
    (expression (boolean) boolean-exp)
    (expression ( identifier "->" expression ) allot-exp)  
    (expression ( "x8" "(" number (arbno "," number ) ")" ) octal-exp) 
    (expression ( "[" (separated-list expression ",") "]" ) list-exp)
    (expression ( "vector" "[" (separated-list expression ";" ) "]" ) vector-exp)
    (expression ("record" "{" identifier "=" expression  (arbno "," identifier "=" expression)  "}") record-exp)
    (expression (expr-bool) bool-exp)
    (expr-bool ( "compare"  pred-prim "(" expression "," expression ")") compare-exp)
    (expr-bool ( oper-bin-bool "(" expr-bool "," expr-bool (arbno "," expr-bool) ")" ) oper-bin-exp)
    (expr-bool ("{" boolean "}") simpe-bool-exp)
    (expr-bool (oper-a-bool "(" expr-bool ")" ) oper-a-exp)
    (expression ( "sequence" "(" expression (arbno ";" expression) ")") sequence-exp)
    (expression ( "if" "(" expr-bool ")" "then" "{" expression "}" "else"  "{" expression "}") if-exp)
    (expression ( "cond" (arbno "[" expr-bool "]" expression ) "else" expression ) cond-exp)
    (expression ( "for" "(" identifier "=" expression ";" decr-incre-exp expression ")" "{" expression "}") for-exp)
    (decr-incre-exp ("to") to-exp)
    (decr-incre-exp ("downto") downto-exp)
    ;numbers' or octals' expressions.
    (expression ("(" expression primitive expression ")" ) primapp-bin-exp)
    (expression (primitive-un "(" expression ")" ) primapp-un-exp)
    ;strings' expressions.
    (expression (primitive-bin-string "(" expression "," expression ")") primapp-bin-string) ;concat  
    (expression (primitive-un-string "(" expression ")" ) primapp-un-string-exp) ;length
    ;lists' expressions.
    (expression (primitive-null-list) null-list-exp)
    (expression (primitive-list "("(separated-list expression ",") ")") primapp-un-list-exp) ;create-list, top, pop, null?
    (expression (primitive-bin-list "(" expression "," expression ")") primapp-bin-list-exp) ;append
    ;vectors' expressions.
    (expression (primitive-vector "(" number expression ")") primapp-vector) ;ref-vector, set-vector.
    (expression ("#"primitive-vector "(" (separated-list expression ",") ")" ) primapp-create-vector) ;create vector.

    ;records' expressions.
    (expression (primitive-bin-record "(" number expression ")" ) primapp-bin-record) ;set-record, ref-record.
    (expression (primitive-un-record "(" expression ")" ) primapp-un-record) ;record?
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
    (primitive-null-list ("null") null-list-prim)
    (primitive-list ("top") head-list-prim)
    (primitive-list ("pop") tail-list-prim)
    (primitive-list ("null?") null-pred-prim)
    (primitive-list ("create-list") create-list-prim)
    (primitive-bin-list ("append") append-list-prim)
    ;vectors
    (primitive-vector ("ref-vector") ref-vector-prim)
    (primitive-vector ("set-vector") set-vector-prim)
    (primitive-vector ("vector?") vector-pred-prim)
    (primitive-vector ("create-vector") create-vector-prim)
     ;records
    (primitive-un-record ("record?") record-pred-prim)
    (primitive-un-record ("create-record") record-recreate-prim)
    (primitive-bin-record ("ref-record") ref-record-prim)
    (primitive-bin-record ("set-record") set-record-prim)
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

