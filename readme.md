# Lynx

An experimental library for dynamic forms. Any release version will exist under the CitizenNet organization, not here. Extremely unstable!

## Quick Start

Install dependencies, build, watch file changes, and serve the database and HTML:

```sh
yarn && open http://localhost:1234 && open http://localhost:3000 && yarn start
```

## Use

After building the application and running the database, navigate to `.html` to see a form constructed with the DSL, saved to the database, and then run.

To install dependencies and run the app:

```sh
yarn        # install all dependencies
yarn start  # build/watch application & run db server / html server
```

To view the application and database:

```sh
open http://localhost:1234  # app entry page
open http://localhost:3000  # database / api summary
```

The project relies on `json-server` to mock dynamic forms being loaded and saved to the database, `parcel` for bundling and building the JS, and `psc-package` for package management.

# Major Todos

- Add a layout DSL that specifies the order of fields and allows you to add extra data (label, etc.)
- Parse a form to a correct type (`User`, for example)
- Support fields as components (not just `Text`)
- Support more complex validation with variants

# Open Questions

- How to support arbitrary HTML in between fields?
  - Layout DSL that allows you to insert HTML blocks of the right type?
