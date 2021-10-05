;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hw5notes3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))
;new
(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))
                              


(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2)) ;integer division 
      ((eq? op '%) (modulo num1 num2)) ;remainder of integer division
      ((eq? op '*) (* num1 num2))
      (else #f))))
      


(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'function)
       (list 'func-exp
             (append (list 'params) (cadr no-code))
             (list 'body
                   (no-parser (caddr no-code)))))
      (else (list 'call-exp
                  (no-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))
    
  
(define env '((age 21) (a 7) (b 5) (c 23)))
;(define sample-no-code '(call (function (x) x) a))
;New
;(extend-env '() '() env)
;New
(define sample-no-code '(call (function (x y) (do-mathy-stuff + x y)) (do-mathy-stuff  * a b) 15)) ;what we want to accomplish,

;(math-exp + )var-exp a) (var-exp b))



(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'func-exp)
       (run-parsed-code (cadr (caddr parsed-no-code)) env))
      (else
       (extend-env
        (cdr (cadr (cadr parsed-no-code)))
        (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
        env))))) ;fuck, list of packets with two lists where the first element will be the parsed code and the second part of it is something
;New, creating a bunch of two lists with one parameter because map, (car packet) is the first packet (cadr packet) is the second part of the packet                    
;packet is list, two list, that is unexecuted parsed code, so like (list 'num-lit-exp 15)))
(define parsed-no-code (no-parser sample-no-code))
parsed-no-code
(run-parsed-code parsed-no-code env)