(named-readtables:in-readtable fn_:fn_lambda)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun remove-0-len (x)
  (remove-if λ(= (length %) 0) x))

(defun defeatist-car (x) (when (listp x) (car x)))
(defun defeatist-cdr (x) (when (listp x) (cdr x)))

(macrolet ((def-defeatist (depth)
             (assert (> depth 1))
             (let ((depth (- depth 1))
                   (r '((:a) (:d)))
                   (last-count 2))
               (loop for i below depth do
                    (let ((pass (loop for e in (subseq r 0 last-count)
                                   collect (cons :a e)
                                   collect (cons :d e))))
                      (setf last-count (length pass))
                      (setf r (append pass r))))
               `(progn
                  ,@(loop for name-spec in (subseq r 0 (- (length r) 2)) collect
                         (let ((fun-name (symb :defeatist-c (apply #'symb name-spec) :r))
                               (prev (symb :defeatist-c (apply #'symb (rest name-spec)) :r))
                               (f (if (eq (first name-spec) :a) 'defeatist-car 'defeatist-cdr)))
                           `(defun ,fun-name (x)
                              (when (listp x) (,f (,prev x))))))))))
  (def-defeatist 4))

(defun apply-tree (func tree)
  (multiple-value-bind (val changed) (funcall func tree)
    (if changed
        val
        (if (listp tree)
            (mapcar (lambda (x) (apply-tree func x)) tree)
            tree))))

(defun moo ()
  (let* ((a1
          (let ((raw (with-open-file (s "./SPIR-V Specification (Provisional).html")
                       (apply-tree (lambda (x)
                                     (when (stringp x)
                                       (values (string-trim '(#\space #\newline) x) t)))
                                   (cl-html-parse:parse-html s)))))
            (third (second raw))))
         (a2 (remove-if λ (= 0 (length %)) (elt a1 4)))
         (a3 (remove-0-len (elt a2 4)))
         (a4 (remove-0-len (elt a3 2)))
         (a5 (find "3.27." a4 :key #'defeatist-cadadr :test #'equal))
         (a6 (remove-0-len a5)))
    (mapcar #'parse-section (subseq a6 4))))


(defun parse-section (x)
  `((:title ,(cadadr x))
    (:tables ,(mapcar #'parse-table (subseq (remove-0-len x) 2)))))

(defun parse-table (x)
  (let* ((tbody (elt x 2))
         (dblock (cadadr tbody))
         (sec-a (cddadr dblock))
         (bottom-row (fourth tbody)))
    `((:name ,(cadar (cddadr dblock)))
      (:description ,(parse-description sec-a))
      (:required-capabilities)
      (:word-count)
      (:opcode)
      (:results)
      (:operands)
      (:tmp ,tbody))))

(defun parse-description (x)
  (format
   nil "~{~a~}"
   (loop for part in (rest x) collect
        (cond ((eq part :br) #\newline)
              (t part)))))

;;raw table
'((:TBODY
   (:TR
    ((:TD :CLASS "tableblock halign-left valign-top" :COLSPAN "2")
     ((:P :CLASS "tableblock") ((:A :ID "OpNop")) (:STRONG "OpNop") :BR ""
      :BR "Use is invalid.")))
   ""
   (:TR
    ((:TD :CLASS "tableblock halign-left valign-top")
     ((:P :CLASS "tableblock") "1"))
    ((:TD :CLASS "tableblock halign-left valign-top")
     ((:P :CLASS "tableblock") "0")))))
