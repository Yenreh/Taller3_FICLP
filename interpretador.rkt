#lang eopl
;******************************************************************************************
;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>      ::= <expresion>
;;                      <un-program (exp)>
;;
;;  <expresion>     ::= <numero>
;;                      <numero-lit (num)>
;;
;;                  := "\""<texto> "\""
;;                      <texto-lit (txt)>
;;
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;
;;                  ::= (<expresion> <primitiva-binaria> <expresion>)
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>
;;
;;                  ::= <primitiva-unaria>(<expresion>)
;;                      <primapp-un-exp (prim-unaria exp)>
;;
;;                  ::= Si <expresion> "{" <expresion> "}" "sino" "{" <expresion> "}"
;;                      <condicional-exp (test-exp true-exp false-exp)>
;;
;;  <primitiva-binaria>   ::= + (primitiva-suma)
;;                        ::= ~ (primitiva-resta)
;;                        ::= / (primitiva-div)
;;                        ::= * (primitiva-multi)
;;                        ::= concat(primitiva-concat)
;;                        ::= > (primitiva-mayor)
;;                        ::= < (primitiva-menor)
;;                        ::= >= (primitiva-mayor-igual)
;;                        ::= <= (primitiva-menor-igual)
;;                        ::= != (primitiva-diferente)
;;                        ::= == (primitiva-comparador-igual)
;;
;;  <primitiva-unaria>   ::= longitud(primitiva-longitud)
;;                       ::= add1(primitiva-add1)
;;                       ::= sub1(primitiva-sub1)
;;                       ::= neg(primitiva-negacion-booleana)
;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'(
  (espacios   (whitespace) skip)
  (comentario     ("%" (arbno (not #\newline))) skip)
  (identificador  ("@" letter (arbno (or letter digit))) symbol)
  (texto        ("\"" letter (arbno (or letter digit "_")) "\"") string)
  (numero       (digit (arbno digit)) number)
  (numero       ("-" digit (arbno digit)) number)
  (numero       (digit (arbno digit) "." digit (arbno digit)) number)
  (numero       ("-" digit (arbno digit) "." digit (arbno digit)) number)
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    ;;Programa
    
    (programa (expresion) un-programa)

    ;;Expresion
    
    (expresion (numero) numero-lit)
    (expresion (identificador) var-exp)
    (expresion (texto) texto-lit)
    (expresion ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (expresion ("Si" expresion "{" expresion "}" "sino" "{" expresion "}") condicional-exp)


    ;;Primitiva Binaria

    (primitiva-binaria ("+")      primitiva-suma)
    (primitiva-binaria ("~")      primitiva-resta)
    (primitiva-binaria ("/")      primitiva-div)
    (primitiva-binaria ("*")      primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">")      primitiva-mayor)
    (primitiva-binaria ("<")      primitiva-menor)
    (primitiva-binaria (">=")     primitiva-mayor-igual)
    (primitiva-binaria ("<=")     primitiva-menor-igual)
    (primitiva-binaria ("!=")     primitiva-diferente)
    (primitiva-binaria ("==")     primitiva-comparador-igual)

    ;;Primitiva Unaria

    (primitiva-unaria ("longitud")  primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)
  )
)
;*******************************************************************************************
;Dataypes generados por SLLGEN

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (evaluar-programa pgm))
    (sllgen:make-stream-parser
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


;*******************************************************************************************
;El Interprete

;evaluar-programa: <programa> -> expresion
;función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (exp)
                 (evaluar-expresion exp (init-env))
      )
    )
  )
)

; Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
      '(@a @b @c @d @e)
      (list 1 2 3 "hola" "FLP")
      (empty-env)
    )
  )
)

;evaluar-expresion:
;evalua la expresión en el ambiente de entrada

(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (numero) numero)
      (var-exp (id) (buscar-variable env id))
      (texto-lit (texto) (clean-string texto))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (evaluar-primitiva-bin  exp1 prim-binaria exp2 env))
      (primapp-un-exp (prim-unaria exp)
                      (evaluar-primitiva-un prim-unaria exp env))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (eqv? (valor-verdad? (evaluar-expresion test-exp env)) 1)
                           (evaluar-expresion true-exp env)
                           (evaluar-expresion false-exp env)))
     )
   )
)

;Funciones auxiliares para aplicar evaluar-expresion a cada elemento de una lista de operandos (expresiones)

;eval-rands:
;realiza el mapeo de la funcion eval-rand a los elementos de la lista

(define eval-rands
  (lambda (exps env)
    (map (lambda (x) (eval-rand x env)) exps)
  )
)

;eval-rand:
;aplica la funcion evaluar-expresion a una expresion.

(define eval-rand
  (lambda (exp env)
    (evaluar-expresion exp env)
  )
)

;evaluar-primitiva-bin:
;evalua la las primitivas binarias en el ambiente

(define evaluar-primitiva-bin
  (lambda (exp1 prim-binaria exp2 env)  
    (cases primitiva-binaria prim-binaria
        (primitiva-suma () (+ (evaluar-expresion exp1 env) (evaluar-expresion exp2 env)))
        (primitiva-resta () (- (evaluar-expresion exp1 env) (evaluar-expresion exp2 env)))
        (primitiva-div () (/ (evaluar-expresion exp1 env) (evaluar-expresion exp2 env)))
        (primitiva-multi () (* (evaluar-expresion exp1 env) (evaluar-expresion exp2 env)))
        (primitiva-concat () (string-append (evaluar-expresion exp1 env) (evaluar-expresion exp2 env)))
        (primitiva-mayor () (valor-verdad? (> (evaluar-expresion exp1 env) (evaluar-expresion exp2 env))))
        (primitiva-menor () (valor-verdad? (< (evaluar-expresion exp1 env) (evaluar-expresion exp2 env))))
        (primitiva-mayor-igual () (valor-verdad? (>= (evaluar-expresion exp1 env) (evaluar-expresion exp2 env))))
        (primitiva-menor-igual () (valor-verdad? (<= (evaluar-expresion exp1 env) (evaluar-expresion exp2 env))))
        (primitiva-diferente () (valor-verdad? (not (= (evaluar-expresion exp1 env) (evaluar-expresion exp2 env)))))
        (primitiva-comparador-igual () (valor-verdad? (= (evaluar-expresion exp1 env) (evaluar-expresion exp2 env))))
    )
  )
)

;evaluar-primitiva-un: <primitiva> <expresion> -> <numero>
;evalua las operaciones unarias y devuelve un numero 0 o 1 para true o false y en el caso de longitud el tamaño del string

(define evaluar-primitiva-un
  (lambda (prim-unaria exp env)
    (cases primitiva-unaria prim-unaria
        (primitiva-longitud () (string-length (evaluar-expresion exp env)))
        (primitiva-add1 () (+ (evaluar-expresion exp env) 1))
        (primitiva-sub1 () (- (evaluar-expresion exp env) 1))
        (primitiva-negacion-booleana () (if (eqv? (valor-verdad? (evaluar-expresion exp env)) 0) 1 0))
    )
  )
)

;*******************************************************************************************
;;Booleanos

;valor-verdad?:
;determina si un valor dado corresponde a un valor booleano falso o verdadero, 1 para verdadero y 0 para falso
(define valor-verdad?
  (lambda (x)
    (if (or (eqv? x 0) (eqv? x #f)) 0 1)
    ))

;*******************************************************************************************
;Procedimientos

;definicion del tipo de dato para los procedimentos
(define-datatype procval procval?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb environment?)))

;apply-procedure:
;evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids body env)
               (evaluar-expresion body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)
  )
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?)
  )
)

;;Predicado para representar cualquier valor.
(define scheme-value? (lambda (v) #t))

;empty-env: <> -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda () (empty-env-record))) ;llamado al constructor de ambiente vacío


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)
   )
 ) 

;buscar-variable: <ambiente><identificador> -> <scheme-value>
;proposito: función que busca un símbolo en un ambiente y devuelve lo que este almacenado.
(define buscar-variable
  (lambda (env id)
    (cases environment env
      (empty-env-record () (eopl:error "Error, la variable no existe"))
      (extended-env-record (ids vals env)
                           (let(
                                 (pos (list-find-position id ids))
                                )                             
                               (
                                if (number? pos)
                                     (list-ref vals pos)
                                     (buscar-variable env id)
                                )
                           )
      )
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position id proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env id)))
      )
    )
  )
)

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente


(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;clean-string: <string>  -> <string>
;funcion que limpia el string de los / extra al utilizarlos para escapat las comillas
(define clean-string
  (lambda (str)
    (substring str 1 (- (string-length str) 1))))

;****************************************************************************************
;Llamado al interpretador
(interpretador)