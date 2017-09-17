;;; test-sniff -- test-sniff -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(req 'sniff)

(ert-deftest sniff-test-plist ()

  (sniff:defmulti test-sniff-plist
                  (lambda (this a b)
                    (glof:get this :type)))

  (sniff:defmethod test-sniff-plist :dog (_ a b)
                   (format "%s %s, Woof!" a b))

  (sniff:defmethod test-sniff-plist :cat (_ a b)
                   (format "%s %s, Meow!" a b))

  (should (equal "I'm dog, Woof!" (test-sniff-plist '(:type :dog) "I'm" "dog")))
  (should (equal "I'm cat, Meow!" (test-sniff-plist '(:type :cat) "I'm" "cat")))
  )

(ert-deftest sniff-test-vector ()

  (sniff:defmulti test-sniff-vector
                  (lambda (this)
                    (colle:first this)))

  (sniff:defmethod test-sniff-vector :plus (this)
                   (+ (colle:second this)
                      (colle:third this)))

  (sniff:defmethod test-sniff-vector :sub (this)
                   (- (colle:second this)
                      (colle:third this)))

  (should (equal 3 (test-sniff-vector [:plus 1 2]))) 
  (should (equal 1 (test-sniff-vector [:sub 5 4])))
  ) 


(ert-deftest sniff-test-fmap ()
  (sniff:defmulti test-sniff-fmap
                  (lambda (f v)
                    (colle:first v)))

  (sniff:defmethod test-sniff-fmap :just (f mv)
                   (pcase mv
                     (`[:just ,v]
                      `[:just ,(funcall f v)])))

  (sniff:defmethod test-sniff-fmap :nothing (f v)
                   [:nothing])

  (should (equal [:just 2] (test-sniff-fmap #'1+ [:just 1] )))
  (should (equal [:nothing] (test-sniff-fmap #'1+ [:nothing]))))


(ert-deftest sniff-test-type ()
  (sniff:defmulti test-sniff-type
                  (lambda (this)
                    (type-of this)))

  (sniff:defmethod test-sniff-type 'vector
                   (v)
                   "vector")

  (sniff:defmethod test-sniff-type 'symbol
                   (v)
                   "symbol")

  (sniff:defmethod test-sniff-type 'cons
                   (v)
                   "cons")

  (should (equal "symbol" (test-sniff-type '())))
  (should (equal "vector" (test-sniff-type [])))
  (should (equal "vector" (test-sniff-type [a b c])))
  (should (equal "cons" (test-sniff-type '(a b c))))
  )

(ert-deftest sniff-test-area ()
  ;; [[https://en.wikibooks.org/wiki/Clojure_Programming/Examples/API_Examples/Multimethod][Clojure Programming/Examples/API Examples/Multimethod - Wikibooks, open books for an open world]]
  (sniff:defmulti test-sniff-area
                  (lambda (this)
                    (glof:get this :Shape)))
  (sniff:defmethod test-sniff-area :Rect
                   (r)
                   (* (glof:get r :wd)
                      (glof:get r :ht)))
  (sniff:defmethod test-sniff-area :Circle
                   (c)
                   (* pi
                      (* (glof:get c :radius)
                         (glof:get c :radius))))
  (sniff:defmethod test-sniff-area :default
                   (_)
                   "No method!")

  (cl-letf ((r '(:Shape :Rect :wd 4 :ht 13))
            (c '(:Shape :Circle :radius 12)))
    (should (cl-equalp 52
                       (test-sniff-area r)))
    (should (cl-equalp (* pi (* 12 12))
                       (test-sniff-area c))))

  (should (cl-equalp (test-sniff-area '(:Shape :Nothing))
                     "No method!"))
  )

(ert-deftest sniff-test-pcase ()
  (sniff:defmulti test-sniff-pcase
                  (lambda (this)
                    (glof:get this :type)))
  (sniff:defmethod test-sniff-pcase :a (`(:type ,a))
                   a)

  (should (eq :a (test-sniff-pcase '(:type :a))))

  )

(ert-deftest sniff-test-function-symbol ()

  (sniff:defmulti test-sniff-function-symbol
                  #'vectorp)

  (sniff:defmethod test-sniff-function-symbol t (_)
                   :vector)
  (sniff:defmethod test-sniff-function-symbol nil (_)
                   :other)

  (should (eq :vector
            (test-sniff-function-symbol [a])))
  (should (eq :other (test-sniff-function-symbol '(a))))
  
  )

;;; test-sniff.el ends here
