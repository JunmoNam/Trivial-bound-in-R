# Trivial-bound-in-R
Codes and data for trivial upper and lower bound computation in R

## Data
Data is korean actor's relation data for build network, it has 3 columns with 6865 rows
Relationship is defined when each actor work together(in movie or drama)

Var1 : name of actors which will do 'from' node's role
Var2 : name of actors which will do 'to' node's role
value : How many times that relationship happened

## Code
Code contains two part : Trivial bound for diameter and radius
First it will search best boundary in grid, and then do computation again by minimum class in grid result dataframe
