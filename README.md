
# Managed State Event Loop Micro-Framework
## for uLisp and Common Lisp

![Managed State Event Loop as Envisioned by AI](docs/managed-state-event-loop.jpg?raw=true)(as envisioned by AI)


With such a small codebase this is less of a framework than it is a development 
strategy.  However, details matter so here we go...

* _Have you ever needed to do multiple things a once ...or appear to?_

* _Do you need to execute code while reading inputs?_

* _Want to avoid messy, dangerous and expensive global state?_

This is for you, a managed-state event loop.  All you do is worry about what 
to do based on the last thing you did.  Everything else is managed, **you
just write pure stateless functions**!

Costing just 200 lisp-cells, this can run at over 2000 functions/second on a
$7 [AdaFruit Trinkey](https://www.adafruit.com/product/4870) or ~10,000,000 on 
a 2020-era laptop.

![managed function state](docs/function-state.svg?raw=true)

You specify how often, and the event loop executes your function passing in the state for you.

![event loop](docs/event-loop.svg?raw=true)

Run as many functions as you want, however often you want.

![multiple functions](docs/multi-functs-sequence.svg?raw=true)

# Installation - uLisp

1. Copy the [LispLibrary.h](LispLibrary.h) file into your uLisp project directory.
2. Enable it like this [http://www.ulisp.com/show?27OV](http://www.ulisp.com/show?27OV)

# Installation - Common Lisp

This works reasonably well with SBCL.  Others not so much due to the lack of
millisecond support.

1. [Install QuickLisp](https://lispcookbook.github.io/cl-cookbook/getting-started.html#install-quicklisp)
2. Load the Local Time Library `(ql:quickload :local-time)`
3. Define this:
```lisp
(defun millis ()
  (let ((tm (local-time:now))) 
    (+ (* 1000 (local-time:timestamp-to-unix tm))) ))
```

The rest of the code is in [managed-state-event-loop.lisp](managed-state-event-loop.lisp)

# Usage

Create a lambda function taking in one parameter of state, which can be
anything, including a list.

Your functions do not need to be assigned to a variable.

Your functions do not need to change state, just accept the param and return
`nil`.

```lisp
(defvar my-do-this1
  (lambda (x) 
    (format t "~a ..~%" x)
    (1+ x) )) ; adds one and returns the state for the next iteration
```

Pass your function to `run-event-loop` in this format:


```lisp
(
 (500         ;; how often to run, in milliseconds.  This runs every half-second
  my-do-this1 ;; your lambda function
  10000)      ;; initial state
 )
```

Pass it to `run-event-loop`:

```lisp
(defvar my-timed-fns '((500      my-do-this1   10000)))

(run-event-loop my-timed-fns)
```

Or here it is without the fancy:

```lisp
(run-event-loop '((500 (lambda (x) (format t "~a ..~%" x) (1+ x)) 10000)))
```

# Example

```lisp

;; every function expects a state, you dont need to do anything with it
(defvar my-do-this1 (lambda (x) (format t "~a ..~%" x) (1+ x)))
(defvar my-do-this2 (lambda (x) (format t "~a .....~%" x) (1+ x)))
(defvar my-do-this3 (lambda (x) (format t "~a ..........~%" x) (1+ x)))

;;                      millis   lambda-fn     initial state
(defvar my-timed-fns '((500      my-do-this1   10000)
                       (1000     my-do-this2   20000)
                       (7000     my-do-this3   30000)))

(run-event-loop my-timed-fns)
```

Console output:

```shell
2352> (run-event-loop my-timed-fns)
10000 ..
20000 .....
30000 ..........
10001 ..
20001 .....
10002 ..
10003 ..
20002 .....
10004 ..
10005 ..
20003 .....
10006 ..
10007 ..
20004 .....
10008 ..
10009 ..
20005 .....
10010 ..
10011 ..
20006 .....
10012 ..
10013 ..
30001 ..........
20007 .....
10014 ..
10015 ..
20008 .....
10016 ..
```
