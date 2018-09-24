# Updating

```
jagen update [<LAYER>|jagen|self]...
```

Updates the specified layers or Jagen itself.

## Synopsis

Specify a list of shell-like patterns of layer names to update. To see all currently defined layers
use the `jagen list layers` command. If nothing is specified then all layers will be updated.

Special names 'jagen' and 'self' can be added to the list to also update the Jagen repository
associated with the project. These special names do not participate in the layer name pattern
matching and should be specified explicitly.

## Examples

Update all currently defined layers:

    jagen update

Update only Jagen:

    jagen update self

Update all layers and Jagen (note that you need to escape `*` from the Shell to pass it to Jagen):

    jagen update self '*'

Update only layers with names starting with 'ab', containing 'cd` and ending with 'ef':

    jagen update 'ab*' '*cd*' '*ef'

