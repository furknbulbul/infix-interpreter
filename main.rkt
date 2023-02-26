
#lang racket

(provide (all-defined-out))


(define := (lambda (var value) (list var value)))

(define -- (lambda args (list 'let args)))
; 10 points
(define @ (lambda (bindings expr)  (append bindings expr)))
; 20 points

;(define split_at_delim (lambda (delim args) 0))
(define (split_at_delim delim args) 
    (foldr (lambda (element next-result)
        (if (equal? element delim)
            (cons '() next-result)
            (cons (cons element (car next-result)) (cdr next-result))))
            (list '()) args))

;; below is the function to parse expression
;count the elements in the list
(define (count_element_in_list lst element)
  (if(empty? lst)
     0
     (if(equal? (car lst) element)
        (+ 1 (count_element_in_list (cdr lst) element))
        (count_element_in_list (cdr lst) element) )))

;remove unnecessary paran
(define (remove_paran args)
    (if (list? args)
         (if (equal? (length args) 1 )
            (remove_paran (car args))
            (map remove_paran args))
             args))
;parse only a binding
(define (parse_binding binding)
     (if(number? (remove_paran(cdr( remove_paran (split_at_delim ':= binding)))))
        (:=  (eval(car(remove_paran (split_at_delim ':= binding)))) (remove_paran (cdr( remove_paran (split_at_delim ':= binding)))))
        (:=  (eval(car(remove_paran (split_at_delim ':= binding)))) (eval(remove_paran(cdr( remove_paran (split_at_delim ':= binding))))))))
;remove last element in list                               
(define (remove_last lst)
  (reverse (cdr (reverse lst))))

;parse list of bindings
(define (parse_binding_list expr)
   (apply -- (map parse_binding (split_at_delim '-- (remove_last expr)))))

;parse expression regarding the precedence
(define (parse_expr expr) 
  (if (or (symbol? (remove_paran expr))(number? (remove_paran expr))(empty? (remove_paran expr)))
      (remove_paran expr)
      
      
      (if (not(equal? 0 (count_element_in_list (remove_paran expr) '+)))
          (append '(+) (map parse_expr (remove_paran(split_at_delim '+ (remove_paran expr)))))
          (if (not (equal? 0 (count_element_in_list (remove_paran expr) '*)))
              (append '(*) (map parse_expr (remove_paran(split_at_delim '* (remove_paran expr)))))
              (if (equal? 1 (count_element_in_list (remove_paran expr) '@))
                  (@ (parse_binding_list (car (remove_paran (split_at_delim '@ (remove_paran expr))))) (list(parse_expr (remove_paran(cdr (remove_paran(split_at_delim '@ (remove_paran expr))))))))
                  (remove_paran expr))))))
  
 
 
; 20 points
(define eval_expr (lambda (expr) (eval (parse_expr expr))))






