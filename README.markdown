`gitit-tasks` is a plugin for [gitit](http://gitit.net/) which allows for convenient task list management.

# Task Lists

Task lists are lists containing at least one task (and then, *all* list items are treated as tasks).

Example:

    # Sample Tasks

    - [ ] Buy some milk and water the plants (better complete this today than tomorrow).
    - [ ] 2011-12-24 Buy Christmas presents (and have this task already on the radar today).
    - [?] This task will have to be completed some time in the future (but does not bother us too much today).
    - [?] 2010-12-24 @John @Jack #Fun #Stuff This should be done someday (oops, in fact John should have done this last Christmas).
    - [x] This task has been completed. Yay!
    - [x] 2011-01-01 This one was ready by the new year!
    - [/] This task has been canceled. Nay :(
    - [/] 2010-12-31 And we even know when this task has been canceled.

Screenshot:

![Screenshot](https://github.com/downloads/catch22/gitit-tasks/Screenshot.png)

Observe that the last task has been highlighted because it is due.

Also note that, by default, only tasks which are *open* are displayed (i.e., tasks of the `[ ]` and `[?]` families).
This can be changed by using the `tasks: all` page meta option:

![Screenshot](https://github.com/downloads/catch22/gitit-tasks/Screenshot2.png)

# Aggregating Tasks

It is often useful to aggregate tasks from multiple wiki pages into a single page. To this end, `gitit-tasks` dynamically replaces every link of the form

    [!tasks](Shopping List)

by the list of all (top-level) tasks on the respective wiki page (here, `Shopping List`).

By default, tasks are only displayed if they are of the `[ ]` family or if they are due.

# Installation

TBD.
