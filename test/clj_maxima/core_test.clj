(ns clj-maxima.core-test
  (:require [clojure.test :refer :all]
            [abclj.core :as cl]
            [clj-maxima.core :refer :all]))

(deftest meval-test
  (testing "meval test"
      (is (cl/cl->clj (cl/funcall (cl/getfunction 'cl/equal)
                                  (meval "integrate(x^2,x)")
                                  (cl/cl-cons '[[maxima/mtimes maxima/simp nil] [[maxima/rat maxima/simp nil] 1 3 nil] [[maxima/mexpt maxima/simp nil] maxima/$x 3 nil] nil]))))))

(deftest displa-test
  (testing "displa test"
    (is (= " 3\nx\n--\n3\n" (-> "integrate(x^2,x)"
                               meval
                               displa)))))

(deftest mevald-test
  (testing "mevald test"
    (is (= (mevald "integrate(x^2,x)")
           " 3\nx\n--\n3\n"))))

(deftest mevalp-test
  (testing "mevalp test"
    (is (= (with-out-str (mevalp "integrate(x^2,x)"))
           " 3\nx\n--\n3\n"))))

(deftest mset-test
  (testing "mset test"
    (do
      (mset $a 2)
      (meval "test(x):= x+1")
      (is (= 3 (meval "test(a)"))))
    (do
      (mset $a 2)
      (meval "test(x):= x+1")
      (is (cl/cl-obj? (meval "test(a)" :cl true))))))

(deftest mlist-test
  (testing "mlist test"
    (is (cl/cl->clj (cl/funcall (cl/getfunction 'cl/equal)
                                (mlist 1 2 3)
                                (cl/cl-cons '[[maxima/mlist nil] 1 2 3 nil]))))
    (is (cl/cl->clj (cl/funcall (cl/getfunction 'cl/equal)
                                (mlist '$a '$b '$c)
                                (cl/cl-cons '[[maxima/mlist nil] maxima/$a maxima/$b maxima/$c nil]))))))

(deftest mexpr-test
  (testing "mexpr test"
    (is (cl/cl->clj (cl/funcall (cl/getfunction 'cl/equal)
                                (mexpr '[** $%e [* $%i $%pi]])
                                (cl/cl-cons '[[maxima/mexpt nil] maxima/$%e [[maxima/mtimes nil] maxima/$%i maxima/$%pi nil] nil]))))))

(deftest funcall-test
  (testing "funcall test"
    (is (= 0 (funcall '$diff '[** $%e [* $%i $%pi]] '$x)))
    (is (= '$X (funcall '$integrate 1 '$x))); notice that CL is case insensitive for symbols and alaways return a captalized version
    ))
