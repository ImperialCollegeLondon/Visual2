git submodule update --init --recursive
git submodule update --recursive --remote
.paket\paket.bootstrapper.exe
.paket\paket.exe install
.paket\paket.exe update
dotnet restore src\Main\Main.fsproj
dotnet restore src\Renderer\Renderer.fsproj
dotnet restore src\Emulator\Emulator.fsproj
