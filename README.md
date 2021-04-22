# clj-maxima

Maxima as a Clojure library, using [ABCLJ](https://github.com/lsevero/abclj).

## But... how?

Maxima is written in Common Lisp, and can be used as a Common Lisp library.
Maxima by itself has a good Common Lisp interop with options to even switch to the Common Lisp REPL and go back to the Maxima REPL.

With the powers of [ABCL](https://abcl.org/) and [ABCLJ](https://github.com/lsevero/abclj) we can interop with Common Lisp software from Clojure very easily, **without any servers or subprocessing**.
Most of the Maxima integrations I've seen depends of having a Maxima installed in your machine (for example [Maxima.jl](https://github.com/nsmith5/Maxima.jl) ), opens the Maxima repl on a subprocess pipe and send commands to it.
This library access the Maxima functions directly through ABCL, everything happens on the same JVM process (clojure, common lisp or maxima code).
You don't even need Maxima installed in your machine to use this library, this library already have Maxima binaries for ABCL which will be extracted and loaded in the Common Lisp environment automatically.

Maxima is an outstanding project. Is the oldest computer program that I know that is used until nowadays (Macsyma dates from the 60's!!).
With this library I hope to improve the clojure current state for scientific computation, and improve Maxima and Common Lisp reach.

## Usage

Maxima and this library relies 100% on Common Lisp interop, which means that most of the time you will be working with Common Lisp data.
Maxima does not directly eval code in Maxima syntax, it always convert to its lisp list syntax, for example the maxima expression `x*sin(a*x)` is evaluated to `((MAXIMA::MTIMES MAXIMA::SIMP) MAXIMA::$X ((MAXIMA::%SIN MAXIMA::SIMP) ((MAXIMA::MTIMES MAXIMA::SIMP) MAXIMA::$A MAXIMA::$X)))` in lisp.
It is perfectly fine to use this lisp syntax in this library along with Common Lisp interop, but things can become ugly pretty quickly.
I recommend to use Maxima code with Maxima syntax, and just use the Lisp syntax when needed.


To use maxima syntax use the `meval` function.

```clojure
(require '[clj-maxima :as m])
(require '[abclj.core :as cl])

(m/meval "integrate(x*sin(a*x),x)")
; ==> returns
;#abclj/cl-cons ((MAXIMA::MTIMES MAXIMA::SIMP) ((MAXIMA::MEXPT MAXIMA::SIMP) MAXIMA::$A -2) ((MAXIMA::MPLUS MAXIMA::SIMP) ((MAXIMA::MTIMES MAXIMA::SIMP) -1 MAXIMA::$A MAXIMA::$X ((MAXIMA::%COS MAXIMA::SIMP) ((MAXIMA::MTIMES MAXIMA::SIMP) MAXIMA::$A MAXIMA::$X))) ((MAXIMA::%SIN MAXIMA::SIMP) ((MAXIMA::MTIMES MAXIMA::SIMP) MAXIMA::$A MAXIMA::$X))))

;eval and displa (pretty print)
(m/mevald "integrate(x*sin(a*x),x)")
; ==> returns 
;sin(a x) - a x cos(a x)
;-----------------------
;           2
;          a
```

## Plotting

Plotting is fully supported, however it **needs gnuplot installed in your system**.
Maxima alone does not have any GUI facilities, when asked for plots it creates arrays of points and passes to a external program.
When you call the plotting functions Maxima will look for the `gnuplot` binary on your `$PATH`, if it does not find will raise an exception.

To install gnuplot (on Debian based distributions):
```bash
sudo apt install gnuplot-x11
```

To plot use:
```clojure
(require '[clj-maxima :as m])
(require '[abclj.core :as cl])

(m/meval "plot2d(x^2,[x,-5,5])") 

;or call the Maxima lisp functions directly using the lisp syntax through abcl java api
(cl/funcall (cl/getfunction 'maxima/$plot2d)
            (cl/cl-cons '[[maxima/mexpt maxima/simp nil] maxima/$x 2 nil])
            (cl/cl-cons '[[maxima/mlist maxima/simp nil] maxima/$x -5 5 nil]))

;or even with the common lisp interpreter
(cl/with-cl '(in-package :maxima)
            '($PLOT2D '((MEXPT SIMP) $X 2) '((MLIST SIMP) $X -5 5)))
```

## Important Links

* [Call Maxima from Common Lisp](https://niitsuma.hatenadiary.org/entry/20080328/1226706399)
* [Introduction to Maxima](https://maxima.sourceforge.io/docs/manual/intromax.html)
* [Lisp and Maxima](https://maxima.sourceforge.io/docs/manual/maxima_165.html#Lisp-and-Maxima)
* [ABCLJ Readme and tests](https://github.com/lsevero/abclj)
* [Maxima download page (binaries and source code)](https://maxima.sourceforge.io/download.html)

## TODOS

* mfuncall
* maxima contrib packages
* tests
* displa to string

## License

GNU General Public License v2.0

(Same as Maxima)
