#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: mbir.scm,v 1.9 2021-01-12 11:57:59-08 - - $
;;
;; -- Despina Patronas dpatrona@ucsc.edu
;; -- Adam Barsness    abarsnes@ucsc.edu
;;
;; NAME
;;    mbir.scm filename.mbir
;;
;; SYNOPSIS
;;    mbir.scm - mini basic interper
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an mbir
;;    program, which is the executed.  Currently it is only printed.
;;
;; Note:  ~a : arg any data type
;; print  ~s : string
;; specs  ~n : newline
;;

(define *DEBUG* #f)
(define *STDIN* (current-input-port))
(define *STDOUT* (current-output-port))
(define *STDERR* (current-error-port))
(define *ARG-LIST* (vector->list (current-command-line-arguments)))

(define *stmt-table*     (make-hash)) ;;DO NOT MODIFY THIS
(define *function-table* (make-hash)) ;;operantors, trig functions, etc
(define *var-table*      (make-hash)) ;;ret value 0 if DNE
(define *array-table*    (make-hash)) ;;holds arrays
(define *label-table*    (make-hash)) ;;address of each line

;;predefined function table
(for-each (lambda (fxn)(hash-set! *function-table*(car fxn)(cadr fxn)))
   `(
        ;; Arithmetic Operands
        (+     ,+     )
        (-     ,-     )
        (*     ,*     )
        (/     ,/     )
        (^     ,expt  )

        ;; Bool Operands
        (<     ,<     )
        (>     ,>     )
        (<=    ,<=    )
        (>=    ,>=    )
        (=     ,=     )
        (!=    ,(lambda (x y) (not (= x y))))

        ;; Trig functions
        (sqrt  ,sqrt)
        (acos  ,acos)
        (asin  ,asin)
        (atan  ,atan)
        (cos   ,cos)
        (sin   ,sin)
        (tan   ,tan)
 
        ;; Other
        ;; Check the array table
        (asub  ,"asub")
        (abs   ,abs)
        (exp   ,exp)
        (log   ,log)
        (round ,round)
        (trunc ,floor)
        (floor ,floor)
        (ceil  ,ceiling)
        ((log 10) ,(lambda (x) (/ (log x ) (log 10.0))))
    )
)


(for-each (lambda (var) (hash-set! *var-table* (car var) (cadr var)))
   `(
        (e    ,(exp 1.0))
        (eof  0.0)
        (nan  ,(/ 0.0 0.0))
        (pi   ,(acos -1.0))
    )
)

(define *RUN-FILE*
    (let-values
        (((dirname basename dir?)
            (split-path (find-system-path 'run-file))))
        (path->string basename)))

(define (die list)
    (for-each (lambda (item) (fprintf *STDERR* "~a " item)) list)
    (fprintf *STDERR* "~n")
    (when (not *DEBUG*) (exit 1)))

(define (dump . args)
    (when *DEBUG*
        (printf "DEBUG:")
        (for-each (lambda (arg) (printf " ~s" arg)) args)
        (printf "~n")))

(define (usage-exit)
    (die `("Usage: " ,*RUN-FILE* " [-d] filename")))

(define (line-number line)
    (car line))

(define (line-label line)
    (let ((tail (cdr line)))
         (and (not (null? tail))
              (symbol? (car tail))
              (car tail))))

(define (line-stmt line)
    (let ((tail (cdr line)))
         (cond ((null? tail) #f)
               ((pair? (car tail)) (car tail))
               ((null? (cdr tail)) #f)
               (else (cadr tail)))))

(define (not-implemented function args . nl)
    (printf "(NOT-IMPLEMENTED: ~s ~s)" function args)
    (when (not (null? nl)) (printf "~n")))

(define NAN (/ 0.0 0.0) )

(define (eval-expr expr)
    ;;evaluate the expr
    (cond
         ;; Is a number?  Convert to float
         ((number? expr) (+ expr 0.0))

         ;; Is a variab?           get variable !var = 0.0
         ((symbol? expr) (hash-ref *var-table* expr 0.0)) 

         ;; Is a pair?
         ((pair? expr)
             
             ;; Check for func == asub
             (let ((func (hash-ref *function-table* (car expr) #f)))
                  (when (equal? func "asub") ;; cond

                  ;; TRUE: Get the array using the name = key
                  (let((array (hash-ref *array-table* (cadar asub) 
                                                            (#f)))

                      ;; Find the index to accces array
                      (index (map eval-expr (cdr expr))))
                      (printf "~a" (vector-ref array index)))))

             ;; Find the func
             (let (( func (hash-ref *function-table* (car expr) #f))
                  ;;?? map eval-expr to rest of list (gets args) 
                  (operand (map eval-expr (cdr expr)))) 

                  (if (not func) ;; Condition 
                       NAN       ;; T
                      (apply func operand)))) ;; F, apply func to args

         ;; Unidentifiable expr. Default condition
         (else NAN)))

(define (interp-dim args continuation)
    ;; Define local variable and hash-set the vector
    (let ( 
         (name (cadar args))     ;; name of vector
         (size (caddar args))    ;; size of vector
         (vec (make-vector (exact-round (eval-expr (caddar args))) 0.0))
         )
         (hash-set! *array-table* name vec) )
    (interp-program continuation))

(define (interp-let args continuation)
    (hash-set! *var-table*
        (car args) (eval-expr (cadr args)))
    (interp-program continuation))

(define (interp-goto args continuation)
    (let ((address (hash-ref *label-table* (car args))))
    (interp-program address)))

(define (interp-if args continuation)
    (cond ((not (eval-expr(car args))) ;; (assume false/not)
        (interp-program continuation)) ;; if false
        (else (let ((address (hash-ref *label-table* (cadr args))))
              (interp-program address))))) ;; else: if true

(define (interp-print args continuation)
    (define (print item)
        (if (string? item)
            (printf "~a" item)
            (printf " ~a" (eval-expr item))))
    (for-each print args)
    (printf "~n");
    (interp-program continuation))

(define (interp-input args continuation)
    (define(hash-var var)
           (let((value (read)))
                 (hash-set! *var-table* var value)))
    (map hash-var args)
    (interp-program continuation))

(for-each (lambda (fn) (hash-set! *stmt-table* (car fn) (cadr fn)))
   `(
        (dim   ,interp-dim)
        (let   ,interp-let)
        (goto  ,interp-goto)
        (if    ,interp-if)
        (print ,interp-print)
        (input ,interp-input)
    ))

(define (interp-program program)
    (when (not (null? program))
          (let ((line (line-stmt (car program)))
                (continuation (cdr program)))
               (if line
                   (let ((func (hash-ref *stmt-table* (car line) #f)))
                        (func (cdr line) continuation))
                   (interp-program continuation)))))

(define (scan-for-labels program)
    (define (get-label line)
        (and (not(null? line))
             (not (null? (cdr line)))
             (cadr line)))
    (when (not (null? program))
        (let ((label (get-label (car program))))
             (when (symbol? label)
                 (hash-set! *label-table* label program)))
        (scan-for-labels (cdr program))))

(define (readlist filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*RUN-FILE* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-program filename program)
    (define (dump-line line)
        (dump (line-number line) (line-label line) (line-stmt line)))
    (dump *RUN-FILE* *DEBUG* filename)
    (dump program)
    (for-each (lambda (line) (dump-line line)) program))

(define (main arglist)
    (cond ((null? arglist)
                (usage-exit))
          ((string=? (car arglist) "-d")
                (set! *DEBUG* #t)
                (printf "~a: ~s~n" *RUN-FILE* *ARG-LIST*)
                (main (cdr arglist)))
          ((not (null? (cdr  arglist)))
                (usage-exit))
          (else (let* ((mbprogfile (car arglist))
                       (program (readlist mbprogfile)))
                (begin (when *DEBUG* (dump-program mbprogfile program))
                       (scan-for-labels program)
                       (interp-program program))))))

(main *ARG-LIST*)

