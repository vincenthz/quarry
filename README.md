quarry
=======


Initialize
----------

Initializing a new quarry repository is easy:

    $ quarry init /path/to/my/repo

File
----

Importing a file:

    $ quarry import /path/to/my/repo
    my-long-tag-number

Adding some tags:

    $ quarry set /path/to/my/repo my-long-tag-number +tag1 +tag2

Removing some tags:

    $ quarry set /path/to/my/repo my-long-tag-number -tag1 -tag2

Changing some tags:

    $ quarry set /path/to/my/repo my-long-tag-number -tag1 -tag2 +tag3 +tag4

Finding content
---------------

By intersection of tags :

    $ quarry find /path/to/my/repo tag1 tag2
    my-long-tag-number1
    my-long-tag-number2
    ...

more to come ..
