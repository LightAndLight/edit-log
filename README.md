# `edit-log`

A structural editor powered by an 'edit log'.

Edit actions append to a log rather than updating a static data structure.
Syntax tree nodes are hashed when they're inserted into log to improve space
usage as the log grows.

My main motivation for using an edit log is to track program history and calculate
diffs between programs. In general, diffing programs is hard and can't always give
correct answers about how the program changed over time. The edit log tracks how
the program has changed, and it's easy to condense that down into a traditional diff.

The project is split into `core`, which implements the log / versioning / undo / redo,
and `editor`, which implements a web-based user interface with Reflex.

I'm currently unhappy with the performance of the web UI for large programs. Larger
programs lead to a long startup time, and the memory usage too high per 'line of code' 
for my taste. Once loaded, though, the editing is fast regardless of the program size.
