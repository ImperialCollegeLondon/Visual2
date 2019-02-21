# VisUAL2

[Visual2 GitHub official Repo](https://github.com/ImperialCollegeLondon/Visual2)

[**Acknowledgements**](https://github.com/ImperialCollegeLondon/Visual2/wiki/Acknowledgements)

[Assembly Language Documentation Repo](https://tomcl.github.io/visual2.github.io/)

Read the [**Wiki**](https://github.com/ImperialCollegeLondon/Visual2/wiki) before you contribute, or for background info.

For a quick start go straight to [Getting Started](#getting-started).



For HLP students thinking parts of their code are better than what is collected here: you are probably correct. The initial code here was pulled together quickly and mix and match was not attempted much. Put forward a proposal to replace part of this with your code by raising an issue; I will comment and almost certainly agree if you are willing to do the interface work.

The rest of the README gives a project and code overview.


## Introduction

This project is loosely based on a starter template from https://github.com/filangel/arm-monaco.

The target language is `F#`, which is transpiled to `Javascript` (`js`) thanks to [Fable](https://fable.io) v2.x. [Electron](https://electronjs.org/) is then used to convert the developed web-app to a cross-platform native application, providing access to platform-level commands (i.e. file-system, path, multiple processes), which are unavailable to (vanilla) browser web-apps.

[Webpack](https://webpack.js.org/) is the module bundler, responsible for the Javascript concatenation and automated building process.

Finally, [Monaco Editor](https://microsoft.github.io/monaco-editor/) is  a self-contained Javascript component that implements a programmer's editor window, with many features, which the `F#` code can interact with.

## Features

This GUI seeks to reimplement in F# the [VisUAL implementation](https://salmanarif.bitbucket.io/visual/) while making many improvements and changes.  The code in this project is designed to be platform-independent with minimal hassle, as was VisUAL, and is distributed as separate binaries for each of the main desktop platforms generated from this project with `yarn pack-all`.

Whereas VisUAL uses Java to achieve platform-independence this project uses Javascript/HTML. However, in order to make code maintainable nearly all of the source is written in F#.

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

Defines the in-project shortcut commands, therefore when we use `yarn <stript_key>` is equivalent
to calling `<script_value>`. For example, in the root of the project, running in the terminal
`yarn launch` is equivalent to running `electron .`.

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

The compile process is controlled by the above `.fsproj` files (defining the F# source) and `./webpack.config.js` which defines how Webpack combines F# outputs for both electron main and electron app processes and where the executable code is put (see above). This is boilerplate which you do not need to change; normally the F# project files are all that needs to be modified.


### `webpack.config.js`

The `Webpack` configuration file, called when `yarn start` is executed, fires a process that watches changes to `src` folder files and transpiles the `F#` source files to `js` automatically on file save.
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

If you just want to run Visual2 go to the [binaries repo](https://github.com/tomcl/V2releases)

1. Follow instructions to install [yarn](https://yarnpkg.com/lang/en/docs/install/) (which tell you to install Node as well).

2. Download and install the latest (2.x) [Dotnet Core SDK](https://www.microsoft.com/net/learn/get-started).  
For Mac users, download and install [Mono](http://www.mono-project.com/download/stable/) from official website (the version from brew is incomplete, may lead to MSB error on step 7).

3. Download & unzip the Visual2 repo, or if contributing clone it locally, or fork it on github and then clone it locally.

4. Navigate to the project root directory (which contains this README) in a command-line interpreter. For Windows usage make sure if possible for convenience that you have a _tabbed_ command-line interpreter that can be started direct from file explorer within a specific directory (by right-clicking on the explorer directory view). That makes things a lot more pleasant. I recommend [_Hyper_](https://github.com/zeit/hyper/releases), for example, runs multiple tabs and will split window between two tabs, great for running start and launch scripts concurrently in a single window. Beware that under Windows `Hyper` uses `ctrl-shift-C`, `ctrl-shift-V` for copy and paste.


5. Fetch the required `npm` packages by executing `yarn install`. This project consistently uses `yarn` Node package manager instead of `npm`.

6. On macOS or linux ensure you have [paket installed](https://fsprojects.github.io/Paket/installation.html). Run `setup.bat` (on Windows) or `sh setup.sh` (on linux or macOS). This downloads and updates the submodules, and installs their packages individually (necessary because of the submodule structure), then restores the global packages. On other systems run the statements in this file (modified if needed for your system) individually. If MSB error occur while running the script (on macOS) and you were using Mono installed by brew previously, run `brew uninstall mono` and refer to step 2 for install Mono correctly).

7. Goto step 10 if all you want to do is to generate uptodate binaries.

8. In a terminal window (for example under `hyper`) compile `fsharp` code to `javascript` using `webpack` by executing `yarn start` (shortcut for `yarn run start`). This runs the `start` script defined in `package.json`. The `start` script  compiles everything once and then watches source files recompiling whenever any change, so it is normal run continuously throughout development. You will need to view the `yarn start` output throughout development since if compile fails the output makes this clear via red-colored text. Although Ionide will also provide error messages on code that does not compile it is possible to miss these when making quick changes.

9. Open your `electron` app in a new terminal tab by running `yarn launch`. This command will start the application and also _hot reload_ it whenever source files are recompiled, or CSS files changed. Therefore it normally also runs continuously through development. The total time from saving an updated F# source file to reload is typically 5s. Make sure you have this development environment working effectively for you: an HD screen without scaling is helpful because it allows your editor, the Visual2 app, and the command windows all to be visible simultaneously. Using *Hyper* `File->Split Horizontally is useful to run `lauch` and `start` concurrently.

10. Run `yarn pack-win, yarn pack-linux, yarn pack-osx` at any time to create a set of system-specific self-contained binaries in `./dist/os-name/*` and a zip in `./dist`. Each binary distribution consists of a portable directory with all dependencies, so use the appropriate one of these if you just want to run Visual2 is to double click and do not need to develop code. For osx, the easiest way to run Visual2 once it has been built is to navigate to `./dist/VisUAL2-darwin-x64` and execute `open -a VisUAL2.app` in terminal. Note that some host-target combinations will not correctly generate: `pack-osx must be executed on os-x. See also [binaries repo](https://github.com/tomcl/V2releases) for instructions on how to run binaries for different platforms.

11. To see console debug printout etc from the running Visual2 app press `Ctrl-Shift-I` to toggle electron dev tools on and note that any F# printout and errors will be displayed under the console tab.

12. See the [Wiki](https://github.com/ImperialCollegeLondon/Visual2/wiki) for more information.

## Reinstalling compiler and libraries

The code requires a global installation of `dotnet` and `node`/`npm`. This does not need changing and is unlikely to cause trouble. Later versions of dotnet SDK or node can usually be installed without trouble


All the dependencies are local and installed by yarn (node modules) or dotnet (dotnet assemblies). 

WARNING: `dotnet` assemblies are cached locally at machine level by dotnet. This sometimes goes wrong leading to strange compilation errors. It can be cured very simply by clearing the `dotnet` assembly caches, which is done in `setup.bat`.

To reinstall the build environment (without changing project code):

```
setup.bat
```

## Packaging VisUAL2 as binaries

After you have compiled code (and checked it works) `yarn pack-all` will run electron packager and generate `./dist/os-name/*` files. See also `run-packager-all.bat` if using windows host to make macOS binary. See [the packaging issue](https://github.com/ImperialCollegeLondon/Visual2/issues/7) for more details of how this has been customised to work. Note that if this breaks you can still run individual targets as below.

Useful shortcuts for specific common target OS:
* `yarn pack-osx` (macOS - but see below if running from windows host)
* `yarn pack-win` (windows)
* `yarn pack-linux` (linux)


**Note on macOS binaries**. These cannot be packaged as DMG (and therefore used) except on a macOS host. On macOS you need to run `yarn make-osx-dmg` which will FIRST run `yarn pack-osx` and then generate the macOS DMG file as `./dist/visual2-osx.dmg`. 



## Dependency Upgrade

* FABLE used is v2.x.
* Electron used is 2.0.8. Upgrade Electron with care noting that as well as the app running OK, the packaging (electron-packager) must work.
* F# is v4.1. Update to v4.2 not required but should be painless when needed.
* Monaco-editor is v0.12, which fixes an annoying markdown display bug. v0.13 does not contain anything more useful AFAIK but the upgrade should be made at some point.
* Node, Yarn. Versions are baked in by `yarn.lock`. Yarn is currently 1.5.1 but update should be fine.
* Dotnet Core SDK is 2.1. Upgrade to 2.x is painless (global install the version you want) and should be without trouble. Later versions may run faster. Dotnet Core SDK downgrade can be achieved by reinstalling a lower version.
* Webpack. Current is v3.11. No idea when/if a major change to this will happen.

## Boilerplate Development

At some point you may want to change the boilerplate that glues this project together. Understanding all of the boilerplate is not normally needed, and takes some time, but it is possible since each part is quite simple and documented:

There are five distinct sources for this:

* `./app/js/monaco-init.js`. This is setup code for Monaco editor and explained in the [Monaco](https://microsoft.github.io/monaco-editor/index.html)  documentation.
* `*.fsproj`. These are the dotnet core project files for F# which should be changed to add F# dependencies or source files. They are simple and well documented in any of the F# Getting Started guides. Also, they can be changed in a GUI by both VS code / Ionide, and Visual Studio. The Ionide GUI change may not work as of May 2018 (try it).
* `./package.json`. This is instructions to Yarn and FABLE (v1.x) to compile the F# source to JS with Node JS dependencies. Well documented by [fable-compiler](https://github.com/fable-compiler/Fable).
* `./webpack.config` Instructions to `webpack` to bundle the JS files generated by FABLE. Well documented by [webpack](https://webpack.js.org/), but note the need to copy files caused by the non-standard Monaco loader.
* `setup.bat` or `setup.sh`. Code to bootstrap and get initial binary dependencies for F# projects dotnet.

Other top-level files (never changed manually):

* `yarn.lock` auto-generated by yarn contains the package versions currently used of all node packages. These can get upgraded by `yarn upgrade`. Upgrading to latest versions is normally but not always trouble-free.
* `paket.lock`, `paket.dependencies`. Files used by Paket to track nuget (.Net) packages. This project has only _development-time_ .Net dependencies and these do not need to be upgraded.
* `Nuget.Config`. Used by NuGet. You _can_ view the project under Visual Studio and change packages in a GUI via NuGet but this is not recommended.
* `ARM.Monaco.Editor.sln`. this solution file allows all three F# projects to be integrated by Visual Studio or Ionode. It is useful for editing code, but compiling is done via FABLE which uses information from `*.fsproj` files and ignores the `*.sln` file.

## Licensing

No license while still closed source in imperialcollege. 

The project (when published) will use a GPLv3 license with possible exceptions granted on application to the project owner. That allows use in commercial products under negotiated conditions, whilst also ensuring that all code remains open.
