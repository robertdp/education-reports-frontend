### Development setup

Requirements:

- NodeJS 6+

Clone the repo, and navigate to the folder in a terminal and run:

```sh
$ npm install --global elm elm-live uglify-js
$ elm-package install
```

Extra commands are included as NPM tasks:

| Command         | Description                              |
| --------------- | ---------------------------------------- |
| `npm run live`  | Start a "live preview" development server and open it in the browser |
| `npm run build` | Builds, optimises and compresses the application. The resulting files can be founder under the `dist/` folder. |

The files that appear under `dist/` are:

| File            | Description                              | Deployable? |
| --------------- | ---------------------------------------- | ----------- |
| `index.html`    | Used for the live preview                | ✗           |
| `app.dev.js`    | JavaScript output for the live preview   | ✗           |
| `app.js`        | JavaScript output from the Elm build process | ✗           |
| `app.min.js`    | Optimised version of `app.js` after being run though UglifyJS | ✓           |
