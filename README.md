
## `srvdir`

Serve up a tasty directory!

Intended to be able to mock responses with files (no code, just plain plain files).

Can also be used to inspect requests with the --verbose option.

### Usage

    srvdir --port INT [--directory STRING] [--verbose] [--disableListing] [--allMethods] [--cors] [--reflectContentType]

### Options

    --port INT              - The port to run the server on
    [--directory STRING]    - The directory to serve (defaults to .)
    [--verbose]             - Verbose mode
    [--disableListing]      - Disable directory listing (enabled by default)
    [--allMethods]          - Respond to all HTTP methods (only GET by default)
    [--cors]                - Send CORS responses (accept all requests)
    [--reflectContentType]  - Match the content-type response header to the accept request header


