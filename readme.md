# Lynx

An experimental library for dynamic forms. Any release version will exist under the CitizenNet organization, not here. Extremely unstable!

## Use

After building the application and running the database, navigate to `index.html` to see a form constructed with the DSL, saved to the database, and then run.

- Install dependencies and build the project with `yarn && yarn build-app`.
- Build / watch the application and run the database server with `yarn watch-app`
- Build / watch the source and run the database server with `yarn watch`
- Run the server with `yarn serve`

## Major Todos

- Add a layout DSL that specifies the order of fields and allows you to add extra data (label, etc.)
- Parse a form to a correct type (`User`, for example)
- Support fields as components (not just `Text`)
- Support more complex validation with variants

## Open Questions

- How to support arbitrary HTML in between fields?
