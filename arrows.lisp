(in-package :cl-arrows)

(defun simple-inserter (insert-fun)
  (lambda (acc next)
    (if (listp next)
        (funcall insert-fun acc next)
        (list next acc))))

(defmacro -> (initial-form &rest forms)
  "Inserts INITIAL-FORM as first argument into the first of FORMS, the result
into the next, etc., before evaluation.  FORMS are treated as list designators."
  (reduce (simple-inserter #'insert-first)
          forms
          :initial-value initial-form))

(defmacro ->> (initial-form &rest forms)
  "Like ->, but the forms are inserted as last argument instead of first."
  (reduce (simple-inserter #'insert-last)
          forms
          :initial-value initial-form))

(defun diamond-inserter (insert-fun)
  (simple-inserter (lambda (acc next)
                     (case (count-if #'<>p next)
                       (0 (funcall insert-fun acc next))
                       (1 (substitute-if acc #'<>p next))
                       (t (let ((r (gensym "R")))
                            `(let ((,r ,acc))
                               ,(substitute-if r #'<>p next))))))))

(defmacro -<> (initial-form &rest forms)
  "Like ->, but if a form in FORMS has one or more symbols named <> as top-level
element, each such symbol is substituted by the primary result of the form
accumulated so far, instead of it being inserted as first argument.  Also known
as diamond wand."
  (reduce (diamond-inserter #'insert-first)
          forms
          :initial-value initial-form))

(defmacro -<>> (initial-form &rest forms)
  "Like -<>, but if a form has no symbol named <>, the insertion is done at the
end like in ->>.  Also known as diamond spear."
  (reduce (diamond-inserter #'insert-last)
          forms
          :initial-value initial-form))

(defun <>p (form)
  "Predicate identifying the placeholders for the -<> and -<>> macros."
  (and (symbolp form)
       (string= form "<>")))

(defun insert-first (arg surround)
  "Inserts ARG into the list form SURROUND as its first argument, after the
operator."
  (list* (car surround)
         arg
         (cdr surround)))

(defun insert-last (arg surround)
  "Inserts ARG into the list form SURROUND as its last argument."
  (append surround (list arg)))

(defmacro as-> (initial-form var &rest forms)
  "Binds INITIAL-FORM to VAR, then successively each of FORMS to VAR, finally
returns the last value of VAR."
  `(let* ,(mapcar (lambda (form)
                    (list var form))
                  (cons initial-form forms))
     ,var))

(defun some-inserter (insert-fun)
  (lambda (acc next)
    (destructuring-bind (let* bindings var) acc
      `(,let* (,@bindings
               (,var (when ,var
                       ,(funcall insert-fun var next))))
         ,var))))

(defun expand-some (initial-form forms insert-fun)
  (let ((var (gensym "some")))
    (reduce (some-inserter (simple-inserter insert-fun))
            forms
            :initial-value `(let* ((,var ,initial-form))
                              ,var))))

(defmacro some-> (initial-form &rest forms)
  "Like ->, but short-circuits to NIL as soon as either INITIAL-FORM or any of
FORMS return nil.  This is like all these forms are lifted to the maybe monad."
  (expand-some initial-form forms #'insert-first))

(defmacro some->> (initial-form &rest forms)
  "Like some->, but with insertion behaviour as in ->>."
  (expand-some initial-form forms #'insert-last))
