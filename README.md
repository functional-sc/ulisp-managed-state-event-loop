
# Managed State Event Loop Micro-Framework
## for uLisp and Common Lisp

![Managed State Event Loop as Envisioned by AI](docs/managed-state-event-loop.jpg?raw=true)(as envisioned by AI)

This is not just a framework: it's a smooth and efficient development strategy.

* _Ever wished you could do multiple things at once or make it look like you can?_

* _Do you need to execute code while reading inputs?_

* _Want to steer clear of messy, dangerous and expensive global state?_

This is for you, a managed-state event loop.  All you need to focus on is what you want
to do based on the last thing you did.  Everything else is managed, **you
just write pure stateless functions** and let the magic happen.

With a compact code-base of just 200 lisp-cells, this can run at over 2000 functions/second on a
$7 [AdaFruit Trinkey](https://www.adafruit.com/product/4870) or a whopping 10,000,000 on 
a 2020-era laptop.

![managed function state](docs/function-state.svg?raw=true)

Specify how often, and the event loop executes your function passing in the state for you.

![event loop](docs/event-loop.svg?raw=true)

Run as many functions as you desire, as frequently as you want.

![multiple functions](docs/multi-functs-sequence.svg?raw=true)

# Installation - uLisp

1. Copy the [LispLibrary.h](LispLibrary.h) file into your uLisp project directory.
2. Enable it like this [http://www.ulisp.com/show?27OV](http://www.ulisp.com/show?27OV)

# Installation - Common Lisp

Works like a charm on SBCL.  Others?  Not so much due to the lack of
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
(defvar my-function1
  (lambda (x)
    (format t "~a ..~%" x)
    (1+ x) )) ; adds one and returns the state for the next iteration
```

Pass your function to `run-event-loop` in this format:


```lisp
(
 (500         ;; how often to run, in milliseconds.  This runs every half-second
  my-function1 ;; your lambda function
  10000)      ;; initial state
 )
```

Pass it to `run-event-loop`:

```lisp
(defvar my-timed-fns '((500      my-function1   10000)))

(run-event-loop my-timed-fns)
```

Or here it is without the fancy:

```lisp
(run-event-loop '((500 (lambda (x) (format t "~a ..~%" x) (1+ x)) 10000)))
```

# Example - Timing

```lisp

;; every function expects a state, you dont need to do anything with it
(defvar my-function1 (lambda (x) (format t "~a ..~%" x) (1+ x)))
(defvar my-function2 (lambda (x) (format t "~a .....~%" x) (1+ x)))
(defvar my-function3 (lambda (x) (format t "~a ..........~%" x) (1+ x)))

;;                      millis   lambda-fn      initial state
(defvar my-timed-fns '((500      my-function1   10000)
                       (1000     my-function2   20000)
                       (7000     my-function3   30000)))

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
```

# Share States with Names!

*"But wait, that's not all!"* 

Use **Shared Named State** to keep your functons small and targeted!  No need
for your functions to live in a silo or become large and unweildly.  Now they
can share state in the same easy format.

In the next example, we show addition functions sharing their state by notating
a name in the 4th optional list item:

```lisp
;; showcase names: add 1, 2 and 3 respectively
(defvar my-function-add1 (lambda (x) (format t "~a .~%" x)   (+ 1 x)))
(defvar my-function-add2 (lambda (x) (format t "~a ..~%" x)  (+ 2 x)))
(defvar my-function-add3 (lambda (x) (format t "~a ...~%" x) (+ 3 x)))

;;                          millis   lambda-fn          initial   NAME
(defvar named-state-list '((500      my-function-add1   1000      foo)
                           (500      my-function-add2   1000      foo)
                           (500      my-function-add1   8800      bar)
                           (500      my-function-add3   8800      bar)))

(run-event-loop named-state-list)
```

![multiple functions](docs/named-state-sequence.svg?raw=true)

Console output:

```shell
2352> (run-event-loop named-state-list)
1000 .
1001 ..
8800 .
8801 ...
1003 .
1004 ..
8804 .
8805 ...
1006 .
1007 ..
8808 .
8809 ...
1009 .
1010 ..
8812 .
8813 ...
1012 .
1013 ..
8816 .
8817 ...
1015 .
1016 ..
8820 .
8821 ...
1018 .
1019 ..
8824 .
8825 ...
```