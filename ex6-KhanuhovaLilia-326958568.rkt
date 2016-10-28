(require (lib "trace.rkt"))

; Global interpreter constants
(define CONTEXT_TYPE 'static) ; can be either 'static or 'dynamic

; Bonus constant - change to #t if you implement the bonus. Keep on #f otherwise.
(define BONUS_ENABLED #f)

; ********************************************************************************************
; * Do not change anything in the code below, until where marked 'Your code should be here'. *
; * You may only change value of user-definitions if you do part 7.                          *
; ********************************************************************************************

; Special keywords - special forms that are implemented in the interpreter
(define special-keywords '(() #t #f lambda nlambda macro if eval))

; Primitive functions - functions that are used as primitives from the Dr. Racket interpreter
(define primitive-functions '(+ - * / < > <= >= <> = eq? equal? null? pair? cons car cdr))

; System context - contains default system functions (non-primitive) and constants - Can add more here
(define system-definitions '((pi 3.14159265358979)
                             (list (lambda x x))
                             (quote (nlambda (x) x))
                             (caar (macro (p) (list 'car (list 'car p))))
                             (cadr (macro (p) (list 'car (list 'cdr p))))
                             (cadar (macro (p) (list 'car (list 'cdr (list 'car p)))))
                             (cond-make-conds (lambda (conds)
                                                (if (null? conds)
                                                    ()
                                                    (if (eq? 'else (caar conds))
                                                        (cadar conds)
                                                        (list 'if (caar conds) (cadar conds)
                                                              (cond-make-conds (cdr conds)))))))
                             (cond (macro conds (cond-make-conds conds)))
                             (map (lambda (pred lst)
                                    (if (null? lst) ()
                                        (cons (pred (car lst)) (map pred (cdr lst))))))
                             (append (lambda (lst1 lst2)
                                       (if (null? lst1) lst2
                                           (cons (car lst1) (append (cdr lst1) lst2)))))
                             (let (macro (defs body) 
                                         (append (list (list 'lambda (map car defs) body))
                                                 (map cadr defs))))
                             ))

; User context - contains user functions (non-primitive) and constants - Can add more here
(define user-definitions '((first (macro (lst) (list 'car lst)))
                           (second (macro (lst) (list 'car (list 'cdr lst))))
                           (third (macro (lst) (list 'car (list 'cdr (list 'cdr lst)))))
                           (fourth (macro (lst) (list 'car (list 'cdr (list 'cdr (list 'cdr lst))))))
                           ; ***********************
                           ; * Add bonus code here *
                           ; ***********************
                           ))

; Makes a context out of a given list of definitions
(define (make-context dict)
  (if (null? dict) ()
      (dict-put (caar dict) (evaluate (cadar dict) ()) (make-context (cdr dict)))))

; Runs user code with an empty initial context
(define (run-code expr)
  (evaluate expr ()))

; Shows a prompt to the user to enter his code to run
(define (show-prompt-loop)
  (display "Enter an expression (type 'exit' to stop):")
  (newline)
  (let ((exp (read)))
    (if (not (eq? exp 'exit))
        (let ((result (run-code exp)))
          (if (not (eq? result (void)))
              (begin
                (display result)
                (newline)))
          (show-prompt-loop)))))

; Dictionary management (from class)
(define (dict-put key value ctx)
  (cons (list key value) ctx))

(define (dict-put-many entries ctx)
  (append entries ctx))

(define (dict-get key ctx)
  (let ((res (assoc key ctx)))
    (if res (cadr res) #f)))

; ***************************************************************************************
; ********************************* Add your code here! *********************************
; ***************************************************************************************


; Name: Khanuhov Lilia
; ID:   326958568

; Part 1: Evaluate Arguments
; This function receives a list of argument and a context.It evaluates each argument
; in the given contex, and returns a list of the result of evaluation.
(define (eval-args args ctx)
  (map (lambda (argument) (evaluate argument ctx)) args))

; Part 2: Bind Parameters to Arguments
; The function binds the given parameters to the given arguments.
; It returns a list of dictionary elements of "parameter argument".
(define (bind params args) 
  (define (helper lst args)
    (if (symbol? lst) 
        (list (list lst args))
        (cons (list (car lst) (car args)) (helper (cdr lst) (cdr args)))))
  (cond ((list? params) (map list params args))
        ((symbol? params) (list (list params args)))
        (else
         (helper params args))))

; Part 3: Evaluating a Symbol
; The function returns the value of the given symbol sym,
; or some special internal representation for future use.
(define (eval-symbol sym ctx)
  (cond ((member sym special-keywords) sym)
        ((member sym primitive-functions) (list '_primitive (eval sym)))
        ((symbol? sym)
         (let ((res (dict-get sym ctx)))
           (if res 
               res
               (let ((res (dict-get sym user-context)))
                 (if res
                     (dict-get sym system-context))))))
        (else '(error “reference to undefined identifier”))))

; Part 4: Evaluating an if Expression
; The function receives the condition expression (to be evaluated),
; and two other expressions – one to evaluate if the result of condition evaluation is not #f (if-true),
; and another to evaluate if the result of condition evaluation is #f (if-false).
; The function returns the result of evaluation.
(define (eval-­if condition if-­true if-­false ctx)
  (if (evaluate condition ctx) 
      (evaluate if-­true ctx)
      (evaluate if-­false ctx)))

; Part 5: Evaluating a function call

; A.
; The function checks if the first element of the func argument is _primitive.
; If it is, the function applys the primitive function
; If not, the function calls the function exec-user-func, with the same parameters.
(define (exec-func func args ctx)
  (if (eq? (car func) '_primitive) 
      (apply (second func) (eval-args args ctx))
      (exec-user-func func args ctx)))

; B.
; This function performs the ‘apply’ stage on a function func and a list of 
; arguments args-list, just like the built-in racket ‘apply’ function. It uses the 
; given context ctx for evaluation.
(define (exec-apply func args-list ctx)
  (apply func (evaluate args-list ctx)))

; C.
; This function is only called by exec-­func in case the func argument represents a user function 
; (which can actually be either a lambda, nlambda or macro expression).
; Choose context, binding parameters(using bind) and addint there to the context and evaluating.
(define (exec-user-func func args ctx)
  (let* ((newContext (if (eq? CONTEXT_TYPE 'static) (fourth func)
                         ctx))
         (bindings (if (eq? (car func) '_user_lambda)
                       (bind (second func) (eval-args args ctx))
                       (bind (second func) args)))
         (context_after_binding (dict-put-many bindings newContext)))
    (if (eq? (car func) '_user_macro)
        (evaluate (evaluate (third func) context_after_binding) ctx)
        (evaluate (third func) context_after_binding))))

; Part 6: Implementing the evaluate function
; This function is the main interpreter function and everything is based on it.
; It receives an expression and evaluating it using all above functions.
(define (evaluate exp ctx)
  (cond ((null? exp) exp)  
        ((symbol? exp) (eval-symbol exp ctx))
        ((not (list? exp)) exp)
        (else 
         (let ((func_exp (evaluate (car exp) ctx)))
           (if (symbol? func_exp)
               (cond ((eq? func_exp 'lambda) (list '_user_lambda (second exp) (third exp) ctx))
                     ((eq? func_exp 'nlambda) (list '_user_nlambda (second exp) (third exp) ctx))
                     ((eq? func_exp 'macro) (list '_user_macro (second exp) (third exp) ctx))
                     ((eq? func_exp 'if) (eval-­if (second exp) (third exp) (fourth exp) ctx))
                     ((eq? func_exp 'eval) (evaluate (evaluate (second exp) ctx) ctx))
                     ((eq? func_exp 'apply) (exec-apply (second exp) (third exp) ctx)))
               (exec-func func_exp (cdr exp) ctx))))))



; ***************************************************************************************
; *           The following lines should appear at the end, BELOW your code!            *
; *                            Do NOT change the code below                             *
; ***************************************************************************************

; Initially create system context
(define system-context (make-context system-definitions))

; Initially create user context
(define user-context (make-context user-definitions))

