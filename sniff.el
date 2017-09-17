;;; sniff -- sniff -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'pcase)

(require 'colle)
(require 'glof)

(cl-defmacro sniff:defmulti (name func)
  (unless (colle:find
           (lambda (table)
             (eq name (glof:get table :name)))
           (get name 'sniff-dispatch-table))
    (cl-letf ((defaultimpl `[:default ,(lambda (&rest _ignore) (error "no such method "))]))
      (put name 'sniff-dispatch-table
           (list :name name :function func
                 :impls `[,defaultimpl]))
      `(cl-defun ,name (&rest args)
         (cl-letf ((method (colle:find
                            (lambda (impl)
                              (eq (colle:first impl)
                                  (apply ,func args)))
                            (glof:get
                             (get ',name 'sniff-dispatch-table)
                             :impls)))
                   (defaultmethod (colle:find
                                   (lambda (impl)
                                     (eq (colle:first impl)
                                         :default))
                                   (glof:get
                                    (get ',name 'sniff-dispatch-table)
                                    :impls))))
           (apply
            (if method
                (colle:second method)
              (colle:second defaultmethod))
            args))))))

(cl-defmacro sniff:defmethod (name dispatch args &rest body)
  `(progn
     (put ',name 'sniff-dispatch-table
          (glof:update (get ',name 'sniff-dispatch-table)
                     :impls
                     (lambda (impls)
                       (if (colle:empty-p impls)
                           (vector (vector ,dispatch
                                           (pcase-lambda ,args ,@body)))
                         (if (colle:find
                              (lambda (impl)
                                (eq ',dispatch (colle:first impl)))
                              impls)
                             (colle:conj
                              (colle:remove
                               (lambda (impl) (eq ',dispatch (colle:first impl)))
                               impls)
                              (vector ,dispatch (pcase-lambda ,args ,@body)))
                           (colle:conj
                            impls
                            (vector ,dispatch (pcase-lambda ,args ,@body))))))))))

(provide 'sniff)

;;; sniff.el ends here
