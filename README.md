# skat-engine

CLI tool for generating code for your project.

The tool copies templates and performs substitutions on its contents based on values passed it from the command line.
SKAT does not contain any templates itself, you must create them all.

## Documentation

For code documentation check the [docs](https://dandro.github.io/skat-engine/) in this repository. For usage and
adapters to languages like JavaScript, check [these docs](https://dandro.github.io/skat-js/)

## Installation

At the moment you need Stack to install the tool. Execute the `stack install` command which should install the binary in
your path.

You can use this example configuration to get started:

```json
{
  "templates": "TEMPLATES_PATH",
  "filenameSeparator": "FILENAME_SEPARATOR_CHAR",
  "output": {
    "FILENAME_KEY": "RELATIVE_PATH"
  }
}
```

Or run the tool with the init command to generate one:
```bash
stack run init
```

## Development

The project runs on Haskell GHC and has been setup with Stack version lts-17.10.

## Generate Haddock Documentation

run the `build-tools/gen-docs.sh` shell script to create the code documentation. This is not to know how to use the
application, but to understand how it is built. It is meant for developers of the application. 
