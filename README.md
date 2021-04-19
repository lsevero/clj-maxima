# clj-maxima

Maxima as a Clojure library

## Usage

```clojure
(require '[clj-maxima :as m])
(require '[abclj.core :as cl])

(m/meval "integrate(x*sin(a*x),x)")
; ==> returns
;#abclj/cl-cons ((MAXIMA::MTIMES MAXIMA::SIMP) ((MAXIMA::MEXPT MAXIMA::SIMP) MAXIMA::$A -2) ((MAXIMA::MPLUS MAXIMA::SIMP) ((MAXIMA::MTIMES MAXIMA::SIMP) -1 MAXIMA::$A MAXIMA::$X ((MAXIMA::%COS MAXIMA::SIMP) ((MAXIMA::MTIMES MAXIMA::SIMP) MAXIMA::$A MAXIMA::$X))) ((MAXIMA::%SIN MAXIMA::SIMP) ((MAXIMA::MTIMES MAXIMA::SIMP) MAXIMA::$A MAXIMA::$X))))

(m/mevald "integrate(x*sin(a*x),x)")
; ==> prints 
;sin(a x) - a x cos(a x)
;-----------------------
;           2
;          a


```

## License

GNU General Public License v2.0

(Same as Maxima)
