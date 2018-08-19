.paket\paket.exe install
git submodule update --init --recursive
git submodule update --recursive --remote
:: .paket\paket.bootstrapper.exe
:: .paket\paket.exe install
:: .paket\paket.exe update
dotnet nuget locals all --clear 
dotnet restore src\Main\Main.fsproj
dotnet restore src\Renderer\Renderer.fsproj
dotnet restore src\Emulator\Emulator.fsproj
yarn install
