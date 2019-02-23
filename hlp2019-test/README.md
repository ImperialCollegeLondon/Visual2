# Sample Code for HLP 2019

## How to use this code

Create a directory structure with the two (forked or cloned) repos side by side.

```
github --> Visual2
       --> hlp2019parsing
```

Run the project under dotnet core (not FABLE) to get test functionality. The parse code here is however compatible with FABLE. Note that the project needs to be opened and built in either Visual Studio or JetBrains Rider.

The project in this repo will pick up emulator source files from Visual2, and can therefore be used to test add-on code to the emulator. You *can* change your local copy of the emulator files, and git will track those changes relative to the original code (better if you do this in a branch of the emulator project). Also, the enclosed parsing code has no dependence of the emulator files so you can equally use it elsewhere.

The emulator files are included because it is expected that many of you will write Visual2 emulator add-ons using this project as a starting point to write code and add tests.

### Code contents

Tokeniser is a simple but useful tokenising function that converts a string into a set of words, aware of symbols, numbers and one-character operators. Numbers are parsed as unsigned numbers (if you need signed detect the - sign separately). The parse function here will work under both FABLE and .NET - beware of using other .Net numeric parse functions, they may not deliver identical results under FABLE.

### Expecto and FsCheck

These packages are included in this project. they work fine under dotnet core, but not under FABLE. It is recommended you test emulator add-ons with the emulator code running under dotnet core, then for GUI work use the same code running under FABLE, as it does in the Visual2 project. Since the emulator runs under both dotnet core and FABLE this will work fine.