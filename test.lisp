(in-package #:cl-user)

(defpackage #:cl-arrows/test
  (:use #:cl #:cl-arrows #:hu.dwim.stefil))

(in-package #:cl-arrows/test)

(defsuite* test-cl-arrows)

(deftest test--> ()
  (is (= (-> 3 /) 1/3))
  (is (= (-> 3 (/)) 1/3))
  (is (= (-> 3 (/ 2)) 3/2))
  (is (= (-> 3 (/ 2) /) 2/3)))

(deftest test-->> ()
  (is (= (->> 3 /) 1/3))
  (is (= (->> 3 (/)) 1/3))
  (is (= (->> 3 (/ 2)) 2/3))
  (is (= (->> 3 (/ 2) /) 3/2)))

(deftest test--<> ()
  (is (= (-<> 3 /) 1/3))
  (is (= (-<> 3 (/)) 1/3))
  (is (= (-<> 3 (/ 2)) 3/2))
  (is (= (-<> 3 (/ 2) /) 2/3))
  (is (= (let ((x 3))
           (-<> (incf x)
                (+ <> <>)))
         8)))

(deftest test--<>> ()
  (is (= (-<>> 3 /) 1/3))
  (is (= (-<>> 3 (/)) 1/3))
  (is (= (-<>> 3 (/ 2)) 2/3))
  (is (= (-<>> 3 (/ 2) /) 3/2))
  (is (= (-<>> (list 1 2 3)
               (remove-if #'oddp <> :count 1 :from-end t)
               (reduce #'+)
               /)
         1/3)))

(deftest test-as-> ()
  (is (= (as-> 3 $
               (* 5 $)
               (/ $ 7))
         15/7)
      (= (as-> 0 n
               (1+ n)
               (1+ n))
         2)))

(deftest test-some-> ()
  (is (null (some-> 3
                    (+ 5)
                    (member '(2 5 9))
                    first
                    (* 9)))
      (= (some-> 3
                 (+ 5)
                 (member '(2 5 8 9))
                 first
                 (* 9))
         72)
      (= (some-> 3
                 (+ 5)
                 (member '(2 5 8 9))
                 second
                 (* 9))
         81)
      (null (some-> 3
                    (+ 5)
                    (member '(2 5 8 9))
                    third
                    (* 9)))))
