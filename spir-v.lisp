(defun apply-tree (func tree)
  (multiple-value-bind (val changed) (funcall func tree)
    (if changed
        val
        (if (listp tree)
            (mapcar (lambda (x) (apply-tree func x)) tree)
            tree))))

(defvar raw-spec
  (apply-tree (lambda (x) (when (stringp x)
                            (values (string-trim '(#\space #\newline) x) t)))
              (cxml:parse-file
               "/Users/Baggers/Documents/SPIR-V/Instructions.html"
               (cxml-xmls:make-xmls-builder))))


(remove-if-not #'listp (elt raw-spec 5))
