unit ce_dubwrap;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, ce_common, ce_writableComponent;

type

  TDubTargetType = ( autodetect, none, executable{, library}, sourceLibrary,
                     staticLibrary, dynamicLibrary);

  TCEDubSubPackageItem = class(TCollectionItem)

  end;

  TCEDubSubPacakges = class(TCollection)

  end;

  TCEDubConfigurationItem = class(TCollectionItem)

  end;

  TCEDubConfigurations = class(TCollection)

  end;

  TCEDubBuildTypeItem = class(TCollectionItem)

  end;

  TCEDubBuildTypes = class(TCollection)

  end;


  (**
   * Warps a DUB JSON project.
   * JSON fields are converted to streamable/inspectable/published properties.
   *
   * the properties must produce the right JSON type when saved with TJSONStreamer.
   *)
  TCEDubProject = class(TWritableJsonComponent)
  private
    fUpdateCount: NativeInt;
    //
    fPackageName: string;
    fDescription: string;
    fHomepage: string;
    fAuthors: string;
    fCopyright: string;
    fLicense: string;
    //
    //fDependencies: ["<name>" : <version-spec>, "<name>" : <version-spec>, ...] TCollection
    fTargetType: TDubTargetType;
    fSystemDependencies: string;
    fTargetName: string;
    fTargetPath: string;
    fWorkingDirectory: string;
    // fSubConfigurations: ["string" : "string", "string": string, ...] TCollection
    fMainSourceFile: string;
    fbuildRequirements: TStringList;
    fbuildOptions: TStringList;
    fLibs: TStringList;
    fSourceFiles: TStringList;
    fSourcePaths: TStringList;
    fExcludedSourceFiles: TStringList;
    fCopyFiles: TStringList;
    fVersions: TStringList;
    fDebugVersions: TStringList;
    fImportPaths: TStringList;
    fStringImportPaths: TStringList;
    fPreGenerateCommands: TStringList;
    fPostGenerateCommands: TStringList;
    fPreBuildCommands: TStringList;
    fPostBuildCommands: TStringList;
    fDflags: TStringList;
    fLflags: TStringList;
    //
    fSubPackages: TCEDubSubPacakges;
    fConfigurations: TCEDubConfigurations;
    fBuildTypes: TCEDubBuildTypes;
    fDdoxFilterArgs: TStringList;
  published

    // global
    property packageName: string read fPackageName;
    property description: string read fDescription;
    property homepage: string read fHomepage;
    property authors: string read fAuthors;
    property copyright: string read fCopyright;
    property license: string read fLicense;

    // common build settings
    //dependencies;
    property systemDependencies: string read fSystemDependencies;
    property targetType: TDubTargetType read fTargetType;
    property targetName: string read fTargetName;
    property targetPath: string read fTargetPath;
    property workingDirectory: string read fWorkingDirectory;
    //subConfigurations;
    property buildRequirements: TStringList read FbuildRequirements;
    property buildOptions: TStringList read fBuildOptions;
    property libs: TStringList read fLibs;
    property sourceFiles: TStringList read fSourceFiles;
    property sourcePaths: TStringList read fSourcePaths;
    property excludedSourceFiles: TStringList read fExcludedSourceFiles;
    property mainSourceFile: string read fMainSourceFile;
    property copyFiles: TStringList read fCopyFiles;
    property versions: TStringList read fVersions;
    property debugVersions: TStringList read fDebugVersions;
    property importPaths: TStringList read fImportPaths;
    property stringImportPaths: TStringList read fStringImportPaths;
    property preGenerateCommands: TStringList read fPreGenerateCommands;
    property postGenerateCommands: TStringList read fPostGenerateCommands;
    property preBuildCommands: TStringList read fPreBuildCommands;
    property postBuildCommands: TStringList read fPostBuildCommands;
    property dflags: TStringList read fDflags;
    property lflags: TStringList read fLflags;

    // collections
    property subPackages: TCEDubSubPacakges read fSubPackages;
    property configurations: TCEDubConfigurations read fConfigurations;
    property buildTypes: TCEDubBuildTypes read fBuildTypes;
    property ddoxFilterArgs: TStringList read fDdoxFilterArgs;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure Update;
    procedure beginUpdate;
    procedure endUpdate;
    //
    procedure getSourcesList(aList: TStringList);

  end;

implementation


constructor TCEDubProject.create(aOwner: TComponent);
begin
  inherited;
end;

destructor TCEDubProject.destroy;
begin
  inherited;
end;

procedure TCEDubProject.beginUpdate;
begin
  fUpdateCount += 1;
end;

procedure TCEDubProject.endUpdate;
begin
  fUpdateCount -= 1;
  if fUpdateCount <= 0 then
    Update;
end;

procedure TCEDubProject.Update;
begin
  fUpdateCount := 0;
end;

procedure TCEDubProject.getSourcesList(aList: TStringList);
begin
  {
    sourceFiles - excluded
    sourcePath - excluded
    auto detection - excluded
  }
end;

end.

