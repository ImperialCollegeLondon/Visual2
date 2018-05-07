# VisUAL2



[**Acknowledgements**](https://github.com/ImperialCollegeLondon/Visual2/wiki/Acknowledgements)

Read the [**Wiki**](https://github.com/ImperialCollegeLondon/Visual2/wiki) before you contribute, or for background info.

For a quick start go straight to [Getting Started](#getting-started).

For HLP students thinking parts of their code are better than what is collected here: you are probably correct. The initial code here was pulled together quickly and mix and match was not attempted much. Put forward a proposal to replace part of this with your code by raising an issue; I will comment and almost certainly agree if you are willing to do the interface work.

The rest of the README gives a project and code overview.

## Introduction

This project is loosely based on a starter template from https://github.com/filangel/arm-monaco.

The target language is `F#`, which is transpiled to `Javascript` (`js`) thanks to [Fable](https://fable.io) v1.x. [Github Electron](https://electronjs.org/) is then used to convert the developed web-app to a cross-platform native application, providing access to platform-level commands (i.e. file-system, path, multiple processes), which are unavailable to (vanilla) browser web-apps.

[Webpack](https://webpack.js.org/) is the module bundler, responsible for the Javascript concatenation and automated building process.

Finally, [Monaco Editor](https://microsoft.github.io/monaco-editor/) is  a self-contained Javascript component that implements a programmer's editor window, with many features, which the `F#` code can interact with.

## Features

This GUI seeks to reimplement in F# the [VisUAL implementation](https://salmanarif.bitbucket.io/visual/) while making many improvements and changes.  The code in this project is designed to be platform-independent with minimal hassle, as was VisUAL, and is distributed as separate binaries for each of the main desktop platforms generated from this project with `yarn run package`.

Whereas VisUAL uses Java to achieve platform-independence this project uses Javascipt/HTML. However, in order to make code maintainable nearly all of the source is written in F#.

The project uses automated tests for its ARM emulator that are constructed using the (open) [VisualRandomTestGen](https://github.com/ImperialCollegeLondon/VisualRandomTestGen) project. See testing in the wiki.




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
        "build": "cd src/Main && dotnet fable webpack --port free -- --config webpack.config.js",
        "package": "electron-packager . visual2 --all --out=dist --no-prune --ignore=/node_modules --ignore=/src --overwrite",
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
| electron app | n/a | `./app/monaco-init.js` | (no change)|
| electron app | `./src/renderer/renderer.fsproj`| `./src/renderer/*.fs` | `renderer.js`|
| electron app | `./src/emulate/emulator.fsproj` | `./src/emulator/*.fs` | `renderer.js`|

The `main.fs` source configures electron start-up and is boilerplate. It is transpiled to the root project directory so it can be automatically picked up by Electron.

The app code is arranged as two F# projects, Emulator and Renderer, each with its own directory. Renderer has Emulator as a dependency. The separation allows all the non-web-based code (which can equally be run and tested under .Net) to be run and tested under F# directly in addition to being transpiled and run under Electron.

The Monaco Editor component has some additional Javascript setup, mainly the syntax highlighting, which has not been ported to F# (though it should be!). All this Javascript code is contained in `monaco-init.js` and run before the renderer code.

Finally the GUI skeleton and the script references that run `monaco-init.js` and then `renderer.js` under Chromium are all contained in the top-level app `index.html` file.

The code that turns the F# project source into `renderer.js` is the FABLE compiler [fable-compiler](http://fable.io/) followed by the Node Webpack bundler that combines multiple Javascript files into a single `renderer.js`. Note that the FABLE compiler is distributed as a node package so gets set up automatically with other Node components.

The compilation process is controlled by the above `.fsproj` files (defining the F# source) and `./webpack.config.js` which defines how Webpack combines F# outputs for both electron main and electron app processes and where the executable code is put (see above). This is boilerplate which you do not need to change; normally the F# project files are all that needs to be modified.


### `webpack.config.js`

The `Webpack` configuration file, called when `yarn run start` is executed, fires a process that watches changes to `src` folder files and transpiles the `F#` source files to `js` automatically on file save.
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

##### `app/js/monaco-init.js`

`Monaco Editor` setup script.

##### `app/js/vs`

This subdirectory is copied by webpack from ./node_modules/monaco-editor/min/vs.

It works around the fact that _packaging tools_ do not understand the non-standard Monaco loader, which loads Monaco editor files. Therefore to make things work the Monaco loader dependencies are all copied to the app directly in this directory. Note that extra code in the `webpack.config` script to allow this.

## Getting Started

1. Install [yarn](https://yarnpkg.com/lang/en/docs/install/).

2. Download & unzip the Visual2 project, or if contributing clone locally, or fork on github and then clone locally, the repo.

3. Navigate to the project root directory (which contains this README) in a command-line interpreter. For Windows usage make sure if possible for convenience that you have a _tabbed_ command-line interpreter that can be started direct from file explorer within a specific directory (by right-clicking on the explorer directory view). That makes things a lot more pleasant. One free option is the excellent [hyper](https://github.com/zeit/hyper).

4. Fetch the required `npm` packages by executing `yarn install`. This project consistently uses `yarn` Node package manager instead of the older and less competent `npm`.

5. Run `setup.bat` (on Windows). This downloads and updates the submodules, and installs their packages individually (necessary because of the submodule structure), then restores the global packages. In particular this code will install a copy of .Net Core on which the FABLE compiler can run.

6. Compile `fsharp` code to `javascript` using `webpack` by executing `yarn run start`. This script compiles everything once and then watches source files recompiling whenever any change, so it normally runs continuously throughout development.

7. Open `electron` application at a new terminal tab by running `yarn run launch`. This command will start the application and also _hot reload_ it whenever source files are recompiled. Therefore it normally runs continuously through development. _Hyper_, for example runs multiple tabs and will split window between two tabs, great for running start and launch scripts concurrently in a single window. 

8. Run `yarn run package` or (from windows, to enav=ble correct OS-X binary generation) `./run-packager.bat` at any time to create a set of system-specific self-contained binaries in `./dist/os-name/*`. Each binary distribution consists of a portable directory with all dependencies, so use the appropriate one of these if you just want to run Visual2 and do not need to develop code.

9. To see console debug printout etc press F12 to toggle electron dev tools on and note that any F# printout and errors will be displayed under the console tab.

10. See the [Wiki](https://github.com/ImperialCollegeLondon/Visual2/wiki) for more information.

## Packaging VisUAL2 as binaries

After you have compiled code (and checked it works) `yarn run package` will run electron packager and generate `./dist/os-name/*` files. See [the packaging issue](https://github.com/ImperialCollegeLondon/Visual2/issues/7) for more details.

For windows hosts and OS-X targets the packager requires admin privileges. `./run-packager.bat` will provide these.

## Dependency Upgrade

* FABLE used is v1.3. FABLE will upgrade to v2.x at some point (2019?) which no doubt break quite a lot.
* Electron used is 1.8.2. Upgrade to new Electron v2 should be painless but has not yet been attempted.
* F# is v4.1. Update to v4.2 not required but should be painless when needed.
* Monaco-editor is v0.10.1. Current version is 0.12 and upgrade to this may possibly fix hover Markdown bugs (and allow Markdown links to display?).
* Node, Yarn. Versions are baked in by `yarn.lock`. Yarn is currently 1.5.1 but update should be fine.
* Webpack. Current is v3.11. No idea when/if a major change to this will happen.

## Boilerplate Development

At some point you may want to change the boilerplate that glues this project together. Understanding all of the boilerplate is not normally needed, and takes some time, but it is possible since each part is quite simple and documented:

There are five distinct sources for this:

* `./app/js/monaco-init.js`. This is setup code for Monaco editor and explained in the [Monaco](https://microsoft.github.io/monaco-editor/index.html)  documentation.
* `*.fsproj`. These are the dotnet core project files for F# which should be changed to add F# dependencies or source files. They are simple and well documented in any of the F# Getting Started guides. Also, they can be changed in a GUI by both VS code / Ionide, and Visual Studio. The Ionide GUI change may not work as of May 2018 (try it).
* `./package.json`. This is instructions to Yarn and FABLE (v1.x) to compile the F# source to JS with Node JS dependencies. Well documented by [fable-compiler](https://github.com/fable-compiler/Fable).
* `./webpack.config` Instructions to `webpack` to bundle the JS files generated by FABLE. Well documented by [webpack](https://webpack.js.org/), but note the need to copy files caused by the non-standard Monaco loader.
* `setup.bat`. Code to bootstrap and get initial binary dependencies for F# projects using paket. Documented by paket and dotnet.
