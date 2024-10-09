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