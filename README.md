# cl-rdbms
RDBMS interface supporting PostgreSQL, SQLite  and SAP HANA

## Overview
cl-rdbms is a database interface for PostgreSQL, SQLite and SAP HANA. It is still work in progress.
There is another Common Lisp project by the same name which is older and much more mature but I was unaware of it when I create
d mine. 

## Examples


## Notes

### Table definition
*	Function `create-tabdef (&key schema name columns constraints)`

#### Column constraints
*	Function `**make-colcon** *label* notnull check default unique references`

#### Primary keys

#### Foreign keys

Foreign keys are currently created as INITIALLY DEFERRED
