# grob - robots grep

A library for parsing and rule checking a robots.txt file. It will follow 30x redirects and also honors the cache
headers and 304 responses (if caching is used).

## Caching

There are other backends planned, but the first one is a basic filesystem cache that uses the wonderful AcidState
library to persist the Haskell data structures to disk - this makes quick loading/flushing native since we are
persisting the parse tree (AttoParsec).

# Directive Parsing Limitations

Grob only (for now) parses the "Disallow" directive and the "Allow" directive. At some point there will be support for
the "*" and "$" directive controls.
