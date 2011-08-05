`gitit-tasks` is a plugin for [gitit](http://gitit.net/) which allows for convenient task list management.

# Task Lists

Example:

    # Sample Tasks

    - [ ] Buy some milk and water the plants (better complete this today than tomorrow).
    - [ ](due:2011-12-24) Buy Christmas presents (but already have it on the radar today).
    - [.] This task will have to be completed some time in the future (but does not bother us too much today).
    - [.](due:2010-12-24) This we should also do someday (oops, in fact we should have done this last Christmas).
    - [x] This task has been completed. Yay!
    - [x](done:2011-01-01) This one was ready by the new year!
    - [/] This task has been canceled. Nay :(
    - [/](done:2010-12-31) And we even know when this task has been canceled.

Screenshot:

![Screenshot](https://github.com/downloads/catch22/gitit-tasks/Screenshot.png)

Observe that, by default, only tasks which are *open* are displayed (i.e., tasks of the `[ ]` and `[.]` families).
Moreover, the last task's due date is highlighted, as it has already passed.

# Aggregating Tasks

It is often useful to aggregate tasks from multiple wiki pages into a single page. To this end, `gitit-tasks` dynamically replaces every link of the form

    [!tasks](Shopping List)

by the list of all (top-level) tasks on the respective wiki page (here, `Shopping List`).

By default, only tasks of the `[ ]` family *as well as due tasks* are displayed.
