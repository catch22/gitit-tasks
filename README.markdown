`gitit-tasks` is a plugin for [gitit](http://gitit.net/) which allows for convenient task list management.

# Installation

    cabal install gitit-tasks

    cd $GITIT
    mkdir -p static/css
    ln -sf ~/.cabal/share/gitit-tasks-0.1/static/css/tasks.css static/css/tasks.css

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

It is often useful to aggregate tasks from multiple wiki pages into a single page (e.g., a user's homepage). There are three variants provided by `gitit-tasks`:

1. Aggregating all tasks which are marked as today and which are not delegated to anyone else, as well as due tasks:

        [!tasks](Some Shared Project)

1. Aggregating only those tasks which are due:

        [!duetasks](Scheduled Tasks)

1. Aggregating all tasks which are marked as today or which are due (as in 1.), but only if they are explicitely delegated to us:

        [!tasksdelegatedtome](Other User's Home Page)

`gitit-tasks` dynamically replaces every link of the form of the above form by the list of relevant (top-level) tasks on the respective wiki page.
