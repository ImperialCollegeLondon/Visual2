# VisUAL2

Coordinating Author: Tom Clarke, EEE Dept, Imperial College London

Acknowledgements: 

* HLP 2018 class
* Thomas Carotti
* Lorenzo Silvestri
* Filos Angelo.

## Introduction

This project is based on a starter template from https://github.com/filangel/arm-monaco.
The target language is `F#`, which is transpiled to `Javascript` (`js`) thanks to [Fable](https://fable.io).
[Github Electron](https://electronjs.org/) is then used to convert the developed web-app to a cross-platform native application,
providing access to platform-level commands (i.e. file-system, path, multiple processes), which are unavailable to
(vanilla) browser web-apps.
[Webpack](https://webpack.js.org/) is the module bundler, responsible for the transpilation and automated building process.
Finally, [Monaco Editor](https://microsoft.github.io/monaco-editor/) is used for as a self-contained javascript component that implements an editor window which your `F#` code can interact with.

## Features

This GUI seeks to reimplement in F# the [VisUAL implementation](https://salmanarif.bitbucket.io/visual/) while making many improvements and changes.  The code in this project is designed to be platform-independent with minimal hassle, as was VisUAL, and will be distributed as separate binaries for each of the main desktop platforms.

Whereas VisUAL uses java to achieve platform-independence this project uses Javascipt/HTML. However, in order to make code maintainable nearly all of the source is written in F#.




## Project Structure: package.json

Electron bundles [Chromium](https://www.chromium.org/) (View) and [node.js](https://nodejs.org/en/) (Engine),
therefore as in every `node.js` project, the `package.json` file specifies the (Node) module dependencies.

* dependencies: node libraries that the executable code (and development code) needs
* dev-dependencies: node libraries only needed by development tools


Additionally, the section `"scripts"`:

```json
{
    ...
    "scripts": {
        "start": "cd src/Main && dotnet fable webpack --port free -- -w --config webpack.config.js",
        "build": "cd src/Main && dotnet fable webpack --port free -- -p --config webpack.config.js",
        "launch": "electron ."
    },
    ...
}
```

Defines the in-project shortcut commands, therefore when we use `yarn run <stript_key>` is equivalent
to calling `<script_value>`. For example, in the root of the project, running in the terminal
`yarn run launch` is equivalent to running `electron .`.

## Code Structure

The source code consists of two distinct sections transpiled separately to Javascript to make a complete [Electron](https://electronjs.org/) application.

* The *electron main process* runs the electron parent process under the desktop native OS, it starts the app process and provides desktop access services to it. 
* The electron client (app) process runs under Chromium in a simulated browser environment (isolated from the native OS). 

Electron thus allows code written for a browser (HTML + Javascript) to be run as a desktop app with the additional capability of desktop filesystem access via communication between the two processes.

Both processes run Javascript under Node.

| Process | Project | Source | Executable Code|
|----------|-----------|-------|----------|
| electron main| `./src/main/main.fsproj` | `./src/main/main.fs` | `./main.js`|
| electron app |  n/a |  `./app/index.html` | (no change)|
| electron app | n/a | `./app/editor.js` | (no change)|
| electron app | `./src/renderer/renderer.fsproj`| `./src/renderer/*.fs` | `renderer.js`|
| electron app | `./src/emulate/emulator.fsproj` | `./src/emulator/*.fs` | `renderer.js`|

The `main.fs` source configures electron start-up and is boilerplate. It is transpiled to the root project directory so it can be automatically picked up by Electron.

The app code is arranged as two F# projects, Emulator and Renderer, each with its own directory. Renderer has Emulator as a dependency. The separation allows all the non-web-based code (which can equally be run and tested under .Net) to be run and tested under F# directly in addition to being transpiled and run under Electron.

The Monaco Editor component has some additional Javascript setup, mainly the syntax highlighting, which has not been ported to F# (though it should be!). All this Javascript code is contained in editor.js and run before the renderer code.

Finally the GUI skeleton and the script references that run editor.js and then renderer.js under chromium are all contained in the top-level app index.html file.

The code that turns the F# projects into `renderer.js` is the FABLE compiler [fable-compiler](http://fable.io/) followed by the Node Webpack bundler that combines multiple Javascript files into a `single renderer.js`. Note that the FABLE compiler is distributed as a node package so gets set up automatically.

The compilation process is controlled by the above `.fsproj` files (defining the F# source) and `./webpack.config.js` which defines how Webpack combines F# outputs for both electron main and electron app processes and where the executable code is put (see above). This is boilerplate which you do not need to change; normally the F# project files are all that needs to be modified.





### `webpack.config.js`

`Webpack` configuration file, called when `yarn run start` is executed, firing a process that watches changes
to `src` folder files and transpiled the `F#` project to `js` on save.
For example, the `main.js` file is generated by src/main/main.fs.

## File Structure

### `src` folder

#### `src/Emulator`

The emulator source `F#` code. This is referenced as subproject, although under FABLE it is compiled uniformly with the renderer.

#### `src/Renderer`

The web-app GUI and Monaco editor interface source code.

#### `src/main`

Contains the F# source for the Electron startup main process code (mostly boilerplate).



### `app` folder

The web-app, view, startup files.

#### `app/index.html`

The markup code for the view.
`src/Renderer/Ref.fs` module accesses the elements defined in this DOM tree.

#### `app/css`

`CSS` code to prettify the `index.html` elements.

#### `app/js`

The `js` scripts loaded by the `index.html`, **after** the DOM elements (statically defined) are rendered.

##### `app/js/editor.js`

`Monaco Editor` setup script.

## Getting Started


1. Fetch `npm` packages by executing `yarn install`. This project consistently uses `yarn` Node package manager instead of the older and less competent `npm`.

2. Run `setup.bat` (on Windows). This downloads and updates the submodules, and installs their packages individually (necessary because of the submodule structure), then restores the global packages. In particular this code will install a copy of .Net Core on which the FABLE compiler can run.

3. Compile `fsharp` code to `javascript` using `webpack` by executing `yarn run start`. This script compiles everything once and then watches source files recompiling whenever any change, so it is normally run continuously through development.

4. Open `electron` application at a new terminal tab by running `yarn run launch`. This command will start the application and also hot reload it whenever source files are recompiled. Therefore it normally runs continuously through development.

5. To see debug printout etc press <F12> to toggle electron dev tools on and note that any F# printout and errors will be displayed under the console tab.
