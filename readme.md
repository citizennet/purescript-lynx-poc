# Lynx

An experimental library for dynamic forms. Any release version will exist under the CitizenNet organization, not here. Extremely unstable!

## Quick Start

Install dependencies, build, watch file changes, and serve the database and HTML:

```sh
# Install dependencies, start the application, and open the app locally
npm install && npm run start && open http://localhost:1234
```

## Use

After building the application and running the database, navigate to `.html` to see a form constructed with the DSL, saved to the database, and then run.

To install dependencies and run the app:

```sh
npm i          # install all dependencies
npm run start  # build/watch application & run db server / html server
```

To view the application and database:

```sh
open http://localhost:1234  # app entry page
open http://localhost:3000  # database / api summary
```

The project relies on `json-server` to mock dynamic forms being loaded and saved to the database, `parcel` for bundling and building the JS, and `psc-package` for package management.

# Major Todos

- Produce a dynamic layout spec
