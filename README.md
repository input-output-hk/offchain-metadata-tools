# Metadata Server

## Running

TODO

## Development

```
# Launch a ghcid session for the given target
make dev target=lib:metadata-lib
make dev target=exe:metadata-server
make dev target=exe:metadata-webhook
# Launch a ghci session for the given target
make repl target=lib:metadata-lib
```

## TODO

- Separate EntryContent from Entry
- Write out generic class that turns a product type into a sum type
