`gitit-tasks` is a plugin for [gitit](http://gitit.net/) which allows for convenient task list management.

# Task Lists #

Syntax:

    - [ ] Buy some milk and water the plants (better complete this today than tomorrow).
    - [ ](due:2011-12-24) Buy christmas presents (but already think about it today).

    - [ ](next) This task will have to be completed some time in the future (but does not bother us too much today).
    - [ ](next,due:2010-12-24) Same here (and we even know when it is due).

    - [x] This task has been completed. Yay!
    - [x](done:2011-01-01) This one was ready by the new year!

    - [/] This task has been canceled. Nay :(
    - [/](done:2010-12-31) And we even know when this task has been canceled.

By default, only tasks which are *open* are displayed (i.e., tasks of the `[ ]` family).

# Aggregating Task Lists #

It is often useful to aggregate tasks from multiple wiki pages into a single page. To this end, `gitit-tasks` dynamically replaces every link of the form

    [!tasks](Shopping List)

by the list of all (top-level) tasks on the respective wiki page (here, `Shopping List`).

By default, only tasks which are *open* and *either not marked up as "next" or due* are displayed.
