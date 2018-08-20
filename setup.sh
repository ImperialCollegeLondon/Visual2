paket install
git submodule update --init --recursive
git submodule update --recursive --remote
dotnet nuget locals all --clean
dotnet restore src/Main/Main.fsproj
dotnet restore src/Renderer/Renderer.fsproj
dotnet restore src/Emulator/Emulator.fsproj
yarn install
