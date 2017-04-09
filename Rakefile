require 'bundler/setup'
require 'albacore'
require 'albacore/tasks/release'
require 'albacore/tasks/versionizer'

Configuration = ENV['CONFIGURATION'] || 'Release'

Albacore::Tasks::Versionizer.new :versioning

desc 'create assembly infos'
asmver_files :assembly_info do |a|
  a.attributes assembly_description: 'A smooth unit test framework for F#',
               assembly_configuration: Configuration,
               assembly_company: 'Logibit AB',
               assembly_copyright: "(c) 2016 by Henrik Feldt, formerly Fuchu by @mausch",
               assembly_version: ENV['LONG_VERSION'],
               assembly_file_version: ENV['LONG_VERSION'],
               assembly_informational_version: ENV['BUILD_VERSION']
  a.handle_config do |proj, conf|
    conf.namespace = conf.namespace + "AsmVer"
    conf
  end
end

desc 'Perform fast build (warn: doesn\'t d/l deps)'
build :quick_compile do |b|
  b.prop 'Configuration', Configuration
  b.logging = 'minimal'
  b.sln     = 'Expecto.Tests/Expecto.Tests.fsproj'
end

desc 'Perform fast netcore build (warn: doesn\'t d/l deps)'
task :quick_compile_netcore do
  system "dotnet", %W|build -c #{Configuration} Expecto.netcore.Tests/Expecto.netcore.Tests.fsproj|
end

task :paket_bootstrap do
  system 'tools/paket.bootstrapper.exe',
      %|4.1.7|,
      clr_command: true unless File.exists? 'tools/paket.exe'
end

task :paket_files do
  sh %{ruby -pi.bak -e "gsub(/namespace Logary.Facade/, 'namespace Expecto.Logging')" paket-files/logary/logary/src/Logary.Facade/Facade.fs}
end

task :restore_quick do
  system 'tools/paket.exe', 'restore', clr_command: true
end

task :restore_dotnetcli do
  system "dotnet", %W|restore Expecto.netcore.Tests/Expecto.netcore.Tests.fsproj|
end

desc 'restore all nugets as per the packages.config files'
task :restore => [:paket_bootstrap, :restore_quick, :paket_files, :restore_dotnetcli]

desc 'Perform full build'
build :compile => [:versioning, :restore, :assembly_info, :build_dotnetcli] do |b|
  b.prop 'Configuration', Configuration
  b.sln = 'Expecto.sln'
end

task :build_dotnetcli => [:versioning, :restore_dotnetcli, :assembly_info] do
  system "dotnet", %W|build -c #{Configuration} -f netstandard1.6 Expecto.netcore/Expecto.netcore.fsproj|
end

directory 'build/clipkg'
directory 'build/pkg'


task :create_nugets_dotnetcli => ['build/clipkg', :build_dotnetcli] do
  system "dotnet", %W|pack -v n --no-build -c #{Configuration} -o build/clipkg /p:Version=#{ENV['NUGET_VERSION']} Expecto.netcore/Expecto.netcore.fsproj|
end


desc 'package nugets - finds all projects and package them'
nugets_pack :create_nugets => ['build/pkg', :versioning, :compile, :create_nugets_dotnetcli] do |p|
  p.configuration = Configuration
  p.files   = FileList['*/*.{csproj,fsproj,nuspec}'].
    exclude(/Tests|Sample|netcore/)
  p.out     = 'build/pkg'
  p.exe     = 'packages/NuGet.CommandLine/tools/NuGet.exe'
  p.with_metadata do |m|
    m.description = 'Expecto is a smooth test framework for F#, cloned from Fuchu with added functionality for making it easier to use.'
    m.authors     = 'Henrik Feldt, Logibit AB, formerly @mausch'
    m.project_url = 'https://github.com/haf/expecto'
    m.icon_url    = 'https://raw.githubusercontent.com/haf/expecto/master/docs/expecto-logo-small.png'
    m.tags        = 'testing fsharp assert expect'
    m.version     = ENV['NUGET_VERSION']
  end
end

namespace :tests do
  task :unit do
    system "dotnet", %W|run -c #{Configuration} --project Expecto.netcore.Tests/Expecto.netcore.Tests.fsproj --summary|
    system "Expecto.Tests/bin/#{Configuration}/Expecto.Tests.exe", '--summary', clr_command: true
    system "Expecto.Tests.CSharp/bin/#{Configuration}/Expecto.Tests.CSharp.exe", '--summary', clr_command: true
  end
end

task :tests => :'tests:unit'
task :'tests:unit' => [ :restore_dotnetcli ]
task :default => [ :compile, :tests, :create_nugets ]

task :ensure_nuget_key do
  raise 'missing env NUGET_KEY value' unless ENV['NUGET_KEY']
end

Albacore::Tasks::Release.new :release,
                             pkg_dir: 'build/pkg',
                             depend_on: [:create_nugets, :ensure_nuget_key],
                             nuget_exe: 'packages/NuGet.CommandLine/tools/NuGet.exe',
                             api_key: ENV['NUGET_KEY']
