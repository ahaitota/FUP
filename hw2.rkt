#lang racket
 
(define (eval-circle cx cy r style)
  (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=~s/>" cx cy r style))
 
(define (eval-rect x y width height style)
  (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=~s/>" x y width height style))
 
(define (eval-line x1 y1 x2 y2 style)
  (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=~s/>" x1 y1 x2 y2 style))
 
(define (eval-params param env [vars (append (env-vars env) (env-locals env))])
  (match param
    [(? number?) param]
    [(? string?) param]
    [(list func params ...) (eval-basic-expr param env)]
    [param (if (equal? param (var-name (car vars)))
                 (var-val (car vars))
                 (eval-params param env (cdr vars)))]))
 
(define (eval-prg-expr name global-env params exprs)
  (if (equal? name (expr-name (car exprs)))
      (map (curryr eval-basic-expr (env (env-vars global-env) (env-exprs global-env) (map var (expr-params (car exprs)) params))) (expr-exprs (car exprs)))
      (eval-prg-expr name global-env params (cdr exprs))))
 
(define (eval-basic-expr my-exp env)
  (match my-exp
    [(list 'circle params ...) (apply eval-circle (map (curryr eval-params env) params))]
    [(list 'rect params ...) (apply eval-rect (map (curryr eval-params env) params))]
    [(list 'line params ...) (apply eval-line (map (curryr eval-params env) params))]
    [(list '+ params ...) (apply + (map (curryr eval-params env) params))]
    [(list '- params ...) (apply - (map (curryr eval-params env) params))]
    [(list '* params ...) (apply * (map (curryr eval-params env) params))]
    [(list '/ params ...) (apply / (map (curryr eval-params env) params))]
    [(list 'floor x) (floor (eval-params x env))]
    [(list 'cos x) (cos (eval-params x env))]
    [(list 'sin x) (sin (eval-params x env))]
    [(list '= x y) (equal? (eval-params x env) (eval-params y env))]
    [(list '< x y) (< (eval-params x env) (eval-params y env))]
    [(list '> x y) (> (eval-params x env) (eval-params y env))]
    [(list 'when bool-exp params ...) (if (eval-basic-expr bool-exp env) (map (curryr eval-params env) params) "")]
    [(list 'if bool-exp x y) (if (eval-basic-expr bool-exp env) (eval-params x env) (eval-params y env))]
    [(list name params ...) (eval-prg-expr name env (map (curryr eval-params env) params) (env-exprs env))]))
 
(struct env (vars exprs locals))
(struct var (name val))
(struct expr (name params exprs))
 
(define (create-env prg [vars '()] [exprs '()])
  (if (empty? prg)
      (env vars exprs '())
      (match (car prg)
        [(list define (list name params ...) rest ...) (create-env (cdr prg) vars (cons (expr name params rest) exprs))]
        [(list define name val) (create-env (cdr prg) (cons (var name val) vars) exprs)])))
 
(define (flatten lst)
  (cond
    ((null? lst) '()) 
    ((not (pair? (car lst))) 
     (cons (car lst) (flatten (cdr lst))))
    (else 
     (append (flatten (car lst)) (flatten (cdr lst))))))
 
(define (execute width height prg expr)
  (define open-tag (format "<svg width=\"~a\" height=\"~a\">" width height))
  (define close-tag (format "</svg>"))
  (define global-env (create-env prg))
  (define return (eval-basic-expr expr global-env))
  (if (string? return)
      (string-append open-tag return close-tag)
      (string-append open-tag (apply string-append (flatten return)) close-tag)))
 
(display (execute 400 400 '()
             '(line 10 20 30 40 "stroke:black;stroke-width:5")))
 
(display (execute 400 400 
             '((define STYLE "fill:red"))
             '(circle 200 200 (floor (/ 200 3)) STYLE)))
 
(define test1
    '((define (start)
        (rect 0 0 100 100 "fill:red")
        (rect 100 0 100 100 "fill:green")
        (rect 200 0 100 100 "fill:blue"))))
 
(display (execute 400 400 test1 '(start)))
 
(define test2
  '((define STYLE "fill:red;opacity:0.2;stroke:red;stroke-width:3")
    (define START 195)
    (define END 10)
    (define (circles x r)
      (when (> r END)
        (circle x 200 r STYLE)
        (circles (+ x (floor (/ r 2))) (floor (/ r 2)))))))
 
(display (execute 400 400 test2 '(circles 200 START)))
 
(define tree-prg
    '((define STYLE1 "stroke:black;stroke-width:2;opacity:0.9")
      (define STYLE2 "stroke:green;stroke-width:3;opacity:0.9")
      (define FACTOR 0.7)
      (define PI 3.14)
      (define (draw x1 y1 x2 y2 len angle)
        (if (> len 30)
            (line x1 y1 x2 y2 STYLE1)
            (line x1 y1 x2 y2 STYLE2))
        (when (> len 20)
          (recur-tree x2 y2 (floor (* len FACTOR)) angle)
          (recur-tree x2 y2 (floor (* len FACTOR)) (+ angle 0.3))
          (recur-tree x2 y2 (floor (* len FACTOR)) (- angle 0.6))))
      (define (recur-tree x1 y1 len angle)
        (draw x1
              y1
              (+ x1 (* len (cos angle)))
              (+ y1 (* len (sin angle)))
              len
              angle))))
 
(display (execute 400 300 tree-prg '(recur-tree 200 300 100 (* PI 1.5))))
 
(provide execute)
