
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

![managed function state](docs/function-state.svg?raw=true)


