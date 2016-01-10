unit ce_dmdwrap;

{$I ce_defines.inc}

interface

uses
  classes, sysutils, process, asyncprocess, ce_common,
  ce_processes, ce_interfaces;

(*

procedure to add a new compiler option:
- the option must be published with a setter proc, in the setter 'doChanged' must be called.
- getOpts must be updated to generate the new option.
- Assign() must be updated to copy the new option. (used when cloning a configuration)

*)

type

  (*****************************************************************************
   * Base class designed to encapsulate some compiler options.
   * A descendant must be able to generate the related options
   * as a string representing the partial switches/arguments.
   *)
  TOptsGroup = class(TPersistent)
  private
    fOnChange: TNotifyEvent;
    procedure doChanged;
  protected
    property onChange: TNotifyEvent read fOnChange write fOnChange;
  public
    procedure getOpts(aList: TStrings; base: TOptsGroup = nil); virtual; abstract;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the DDoc and JSON generation.
   *)
  TDocOpts = class(TOptsGroup)
  private
    fGenDoc: boolean;
    fDocDir: TCEPathname;
    fGenJson: boolean;
    fJsonFname: TCEFilename;
    procedure setGenDoc(const aValue: boolean);
    procedure setGenJSON(const aValue: boolean);
    procedure setDocDir(const aValue: TCEPathname);
    procedure setJSONFile(const aValue: TCEFilename);
  published
    property generateDocumentation: boolean read fGenDoc write setGenDoc default false;
    property generateJSON: boolean read fGenJson write setGenJSON default false;
    property DocumentationDirectory: TCEPathname read fDocDir write setDocDir;
    property JSONFilename: TCEFilename read fJsonFname write setJSONFile;
  public
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(aList: TStrings; base: TOptsGroup = nil); override;
  end;


  (*****************************************************************************
   * Describes the different deprecation handling.
   *)
  TDepHandling = (silent, warning, error);

  (*****************************************************************************
   * Encapsulates the options/args related to the compiler output messages.
   *)
  TMsgOpts = class(TOptsGroup)
  private
    fDepHandling : TDepHandling;
    fVerbose: boolean;
    fWarnings: boolean;
    fWarnEx: boolean;
    fVtls: boolean;
    fQuiet: boolean;
    fVgc: boolean;
    fCol: boolean;
    procedure setDepHandling(const aValue: TDepHandling);
    procedure setVerbose(const aValue: boolean);
    procedure setWarnings(const aValue: boolean);
    procedure setWarnEx(const aValue: boolean);
    procedure setVtls(const aValue: boolean);
    procedure setQuiet(const aValue: boolean);
    procedure setVgc(const aValue: boolean);
    procedure setCol(const aValue: boolean);
  published
    property depreciationHandling: TDepHandling read fDepHandling write setDepHandling default warning;
    property verbose: boolean read fVerbose write setVerbose default false;
    property warnings: boolean read fWarnings write setWarnings default true;
    property additionalWarnings: boolean read fWarnEx write setWarnEx default false;
    property tlsInformations: boolean read fVtls write setVtls default false;
    property quiet: boolean read fQuiet write setQuiet default false;
    property showHiddenAlloc: boolean read fVgc write setVgc default false;
    property showColumnsNumber: boolean read fCol write setCol default false;
  public
    constructor create;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(aList: TStrings; base: TOptsGroup = nil); override;
  end;

  (**
   * Describes the target registry size.
   *)
  TTargetSystem = (auto, os32bit, os64bit);

  (**
   * Describes the bounds check kinds.
   *)
  TBoundCheckKind = (onAlways, safeOnly, offAlways);

  (*****************************************************************************
   * Encapsulates the options/args related to the analysis & the code gen.
   *)
  TOutputOpts = class(TOptsGroup)
  private
    fTrgKind: TTargetSystem;
    fBinKind: TProjectBinaryKind;
    fUnittest: boolean;
    fVerIds: TStringList;
    fInline: boolean;
    fBoundsCheck: TBoundCheckKind;
    fOptimz: boolean;
    fGenStack: boolean;
    fAddMain: boolean;
    fRelease: boolean;
    fAllInst: boolean;
    fStackStomp: boolean;
    fAlwayLinkLibs: boolean;
    procedure setAlwaysLinkLibs(const aValue: boolean);
    procedure setAllInst(const aValue: boolean);
    procedure setUnittest(const aValue: boolean);
    procedure setTrgKind(const aValue: TTargetSystem);
    procedure setBinKind(const aValue: TProjectBinaryKind);
    procedure setInline(const aValue: boolean);
    procedure setBoundsCheck(const aValue: TBoundCheckKind);
    procedure setOptims(const aValue: boolean);
    procedure setGenStack(const aValue: boolean);
    procedure setAddMain(const aValue: boolean);
    procedure setRelease(const aValue: boolean);
    procedure setVerIds(const aValue: TStringList);
    procedure setStackStomp(const aValue: boolean);
  published
    property alwaysLinkStaticLibs: boolean read fAlwayLinkLibs write setAlwaysLinkLibs default false;
    property targetKind: TTargetSystem read fTrgKind write setTrgKind default auto;
    property binaryKind: TProjectBinaryKind read fBinKind write setBinKind default executable;
    property inlining: boolean read fInline write setInline default false;
    property boundsCheck: TBoundCheckKind read fBoundsCheck write setBoundsCheck default safeOnly;
    property optimizations: boolean read fOptimz write setOptims default false;
    property generateStackFrame: boolean read fGenStack write setGenStack default false;
    property addMain: boolean read fAddMain write setAddMain default false;
    property release: boolean read fRelease write setRelease default false;
    property unittest: boolean read fUnittest write setUnittest default false;
    property versionIdentifiers: TStringList read fVerIds write setVerIds;
    property generateAllTmpCode: boolean read fAllInst write setAllInst default false;
    property addStackStompCode: boolean read fStackStomp write setStackStomp default false;
  public
    constructor create;
    destructor destroy; override;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(aList: TStrings; base: TOptsGroup = nil); override;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the debugging
   *)
  TDebugOpts = class(TOptsGroup)
  private
    fDebug: boolean;
    fDbgD: boolean;
    fDbgC: boolean;
    fGenMap: boolean;
    fDbgIdents: TStringList;
    fDbgLevel: Integer;
    fForceDbgBool: boolean;
    procedure updateForceDbgBool;
    procedure setDebug(const aValue: boolean);
    procedure setDbgD(const aValue: boolean);
    procedure setDbgC(const aValue: boolean);
    procedure setGenMap(const aValue: boolean);
    procedure setDbgLevel(const aValue: Integer);
    procedure setDbgIdents(aValue: TStringList);
  published
    property debug: boolean read fDebug write setDebug default false;
    property debugIdentifiers: TStringList read fDbgIdents write setDbgIdents;
    property debugLevel: Integer read fDbgLevel write setDbgLevel default 0;
    property codeviewDexts: boolean read fDbgD write setDbgD default false;
    property codeviewCformat: boolean read fDbgC write setDbgC default false;
    property generateMapFile: boolean read fGenMap write setGenMap default false;
  public
    constructor create;
    destructor destroy; override;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(aList: TStrings;base: TOptsGroup = nil); override;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the output and include paths
   *)
  TPathsOpts = class(TOptsGroup)
  private
    fExtraSrcs: TStringList;
    fImpMod: TStringList;
    fImpStr: TStringList;
    fExcl: TStringList;
    fFname: TCEFilename;
    fObjDir: TCEPathname;
    procedure setFname(const aValue: TCEFilename);
    procedure setObjDir(const aValue: TCEPathname);
    procedure setSrcs(aValue: TStringList);
    procedure setIncl(aValue: TStringList);
    procedure setImpt(aValue: TStringList);
    procedure setExcl(aValue: TStringList);
    procedure strLstChange(sender: TObject);
  published
    property outputFilename: TCEFilename read fFname write setFname;
    property objectDirectory: TCEPathname read fObjDir write setObjDir;
    property exclusions: TStringList read fExcl write setExcl;
    property extraSources: TStringList read fExtraSrcs write setSrcs;
    property importModulePaths: TStringList read fImpMod write setIncl;
    property importStringPaths: TStringList read fImpStr write setImpt;
  public
    constructor create;
    destructor destroy; override;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(aList: TStrings; base: TOptsGroup = nil); override;
  end;

  (*****************************************************************************
   * Encapsulates the unclassified and custom options/args
   *)
  TOtherOpts = class(TOptsGroup)
  private
    fCustom: TStringList;
    procedure setCustom(aValue: TStringList);
  published
    property customOptions: TStringList read fCustom write setCustom;
  public
    constructor create;
    destructor destroy; override;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(aList: TStrings; base: TOptsGroup = nil); override;
  end;

  (*****************************************************************************
   * Encapsulates the most common TProcess options.
   * Used to simplify pre/post-compilation and run process options.
   *)
  TCustomProcOptions = class(TOptsGroup)
  private
    fExecutable: TCEFilename;
    fWorkDir: TCEPathname;
    fOptions: TProcessOptions;
    fParameters: TStringList;
    fShowWin: TShowWindowOptions;
    procedure setExecutable(const aValue: TCEFilename);
    procedure setWorkDir(const aValue: TCEPathname);
    procedure setOptions(const aValue: TProcessOptions);
    procedure setParameters(aValue: TStringList);
    procedure setShowWin(const aValue: TShowWindowOptions);
  protected
    property executable: TCEFilename read fExecutable write setExecutable;
    property workingDirectory: TCEPathname read fWorkDir write setWorkDir;
    property options: TProcessOptions read fOptions write setOptions default [];
    property parameters: TStringList read fParameters write setParameters;
    property showWindow: TShowWindowOptions read fShowWin write setShowWin default swoNone;
  public
    constructor create;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure getOpts(aList: TStrings; base: TOptsGroup = nil); override;
    { TAsyncProcess "Parameters" inherits from UTF8 process,
      and the property reader is not anymore "fParameters" but "fUTF8Parameters"
      without the overload aProcess does not get the Parameters if aProcess is TAsynProcess...}
    procedure setProcess(var aProcess: TProcess);
    procedure setProcess(var aProcess: TAsyncProcess);
    procedure setProcess(var aProcess: TCheckedAsyncProcess);
    procedure setProcess(var aProcess: TCEProcess);
  end;

  (*****************************************************************************
   * Encapsulates the options for the pre/post compilation processes
   *)
  TCompileProcOptions = class(TCustomProcOptions)
  published
    property executable;
    property workingDirectory;
    property options default [];
    property parameters;
    property showWindow default swoNone;
  end;

  (*****************************************************************************
   * Encapsulates the options for the project run process.
   * 'executable' prop is hidden since it's defined by the project.
   *)
  TProjectRunOptions = class(TCustomProcOptions)
  published
    property workingDirectory;
    property options default [];
    property parameters;
    property showWindow default swoNone;
  end;

  (*****************************************************************************
   * Encapsulates all the contextual options/args
   *)
  TCompilerConfiguration = class(TCollectionItem)
  private
    fName: string;
    fOnChanged: TNotifyEvent;
    fDocOpts: TDocOpts;
    fDebugOpts: TDebugOpts;
    fMsgOpts: TMsgOpts;
    fOutputOpts: TOutputOpts;
    fPathsOpts: TPathsOpts;
    fOthers: TOtherOpts;
    fPreProcOpt: TCompileProcOptions;
    fPostProcOpt: TCompileProcOptions;
    fRunProjOpt: TProjectRunOptions;
    fIsBaseConfiguration: boolean;
    fIsOverriddenConfiguration: boolean;
    procedure doChanged;
    procedure subOptsChanged(sender: TObject);
    procedure setName(const aValue: string);
    procedure setDocOpts(const aValue: TDocOpts);
    procedure setDebugOpts(const aValue: TDebugOpts);
    procedure setMsgOpts(const aValue: TMsgOpts);
    procedure setOutputOpts(const aValue: TOutputOpts);
    procedure setPathsOpts(const aValue: TPathsOpts);
    procedure setOthers(const aValue: TOtherOpts);
    procedure setPreProcOpt(const aValue: TCompileProcOptions);
    procedure setPostProcOpt(const aValue: TCompileProcOptions);
    procedure setRunProjOpt(const aValue: TProjectRunOptions);
    procedure setisBaseConfiguration(const aValue: boolean);
    procedure setisOverriddenConfiguration(const aValue: boolean);
  protected
    function nameFromID: string;
  published
    property name: string read fName write setName;
    property documentationOptions: TDocOpts read fDocOpts write setDocOpts;
    property debugingOptions: TDebugOpts read fDebugOpts write setDebugOpts;
    property messagesOptions: TMsgOpts read fMsgOpts write setMsgOpts;
    property outputOptions: TOutputOpts read fOutputOpts write setOutputOpts;
    property pathsOptions: TPathsOpts read fPathsOpts write setPathsOpts;
    property otherOptions: TOtherOpts read fOthers write setOthers;
    property preBuildProcess: TCompileProcOptions read fPreProcOpt write setPreProcOpt;
    property postBuildProcess: TCompileProcOptions read fPostProcOpt write setPostProcOpt;
    property runOptions: TProjectRunOptions read fRunProjOpt write setRunProjOpt;
    property isBaseConfiguration: boolean read fIsBaseConfiguration write setisBaseConfiguration default false;
    property isOverriddenConfiguration: boolean read fIsOverriddenConfiguration write setisOverriddenConfiguration default false;
  public
    constructor create(aCollection: TCollection); override;
    destructor destroy; override;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(aList: TStrings; base: TCompilerConfiguration = nil);
    property onChanged: TNotifyEvent read fOnChanged write fOnChanged;
  end;

implementation

uses
  ce_symstring;

procedure TOptsGroup.doChanged;
begin
  if assigned(fOnChange) then fOnChange(self);
end;

{$REGION TDocOpts --------------------------------------------------------------}
procedure TDocOpts.getOpts(aList: TStrings; base: TOptsGroup = nil);
var
  baseopt: TDocOpts;
begin
  if base.isNil then
  begin
    if fGenDoc then
      aList.Add('-D');
    if fGenJson then
      aList.Add('-X');
    if fDocDir <> '' then
      aList.Add('-Dd' + symbolExpander.get(fDocDir));
    if fJsonFname <> '' then
      aList.Add('-Xf' + symbolExpander.get(fJsonFname));
  end else
  begin
    baseopt := TDocOpts(base);
    if baseopt.fGenDoc or fGenDoc then
      aList.Add('-D');
    if baseopt.fGenJson or fGenJson then
      aList.Add('-X');
    if (baseopt.fDocDir <> '') and (fDocDir <> '') then
      aList.Add('-Dd' + symbolExpander.get(fDocDir))
    else if (fDocDir <> '') then
      aList.Add('-Dd' + symbolExpander.get(fDocDir))
    else if (baseopt.fDocDir <> '') then
      aList.Add('-Dd' + symbolExpander.get(baseopt.fDocDir));
    if (baseopt.fJsonFname <> '') and (fJsonFname <> '') then
      aList.Add('-Xf' + symbolExpander.get(fJsonFname))
    else if fJsonFname <> '' then
      aList.Add('-Xf' + symbolExpander.get(fJsonFname))
    else if (baseopt.fJsonFname <> '') then
      aList.Add('-Dd' + symbolExpander.get(baseopt.fJsonFname));
  end;
end;

procedure TDocOpts.assign(aValue: TPersistent);
var
  src: TDocOpts;
begin
  if (aValue is TDocOpts) then
  begin
    src       := TDocOpts(aValue);
    //
    fGenDoc   := src.fGenDoc;
    fGenJson  := src.fGenJson;
    fDocDir   := patchPlateformPath(src.fDocDir);
    fJsonFname:= patchPlateformPath(src.fJsonFname);
  end
  else inherited;
end;

procedure TDocOpts.setGenDoc(const aValue: boolean);
begin
  if fDocDir <> '' then
  begin
    fGenDoc := true;
    exit;
  end;
  //
  if fGenDoc = aValue then
    exit;
  fGenDoc := aValue;
  doChanged;
end;

procedure TDocOpts.setGenJSON(const aValue: boolean);
begin
  if fJsonFname <> '' then
  begin
    fGenJson := true;
    exit;
  end;
  //
  if fGenJson = aValue then
    exit;
  fGenJson := aValue;
  doChanged;
end;

procedure TDocOpts.setDocDir(const aValue: TCEPathname);
begin
  if fDocDir = aValue then
    exit;
  fDocDir := patchPlateformPath(aValue);
  if fDocDir <> '' then
    setGenDoc(true);
  doChanged;
end;

procedure TDocOpts.setJSONFile(const aValue: TCEFilename);
begin
  if fJsonFname = aValue then
    exit;
  fJsonFname := patchPlateformPath(aValue);
  if fJsonFname <> '' then
    setGenJSON(true);
  doChanged;
end;
{$ENDREGION}

{$REGION TMsgOpts --------------------------------------------------------------}
constructor TMsgOpts.create;
begin
  fDepHandling := TDepHandling.warning;
  fWarnings := true;
end;

procedure TMsgOpts.getOpts(aList: TStrings; base: TOptsGroup = nil);
var
  dep, depbase: string;
  baseopt: TMsgOpts;
const
  DepStr : array[TDepHandling] of string = ('-d', '', '-de');
begin
  if base.isNil then
  begin
    dep := DepStr[fDepHandling];
    if dep <> '' then aList.Add(dep);
    if fVerbose then aList.Add('-v');
    if fWarnings then aList.Add('-w');
    if fWarnEx then aList.Add('-wi');
    if fVtls then aList.Add('-vtls');
    if fQuiet then aList.Add('-quiet');
    if fVgc then aList.Add('-vgc');
    if fCol then aList.Add('-vcolumns');
  end else
  begin
    baseopt := TMsgOpts(base);
    dep := DepStr[fDepHandling];
    depbase := DepStr[baseopt.fDepHandling];
    if dep <> depbase then aList.Add(dep) else aList.Add(depbase);
    if baseopt.fVerbose or fVerbose then aList.Add('-v');
    if baseopt.fWarnings or fWarnings then aList.Add('-w');
    if baseopt.fWarnEx or fWarnEx then aList.Add('-wi');
    if baseopt.fVtls or fVtls then aList.Add('-vtls');
    if baseopt.fQuiet or fQuiet then aList.Add('-quiet');
    if baseopt.fVgc or fVgc then aList.Add('-vgc');
    if baseopt.fCol or fCol then aList.Add('-vcolumns');
  end;
end;

procedure TMsgOpts.assign(aValue: TPersistent);
var
  src: TMsgOpts;
begin
  if (aValue is TMsgOpts) then
  begin
    src := TMsgOpts(aValue);
    //
    fDepHandling := src.fDepHandling;
    fVerbose  := src.fVerbose;
    fWarnings := src.fWarnings;
    fWarnEx   := src.fWarnEx;
    fVtls     := src.fVtls;
    fQuiet    := src.fQuiet;
    fVgc      := src.fVgc;
    fCol      := src.fCol;
  end
  else inherited;
end;

procedure TMsgOpts.setDepHandling(const aValue: TDepHandling);
begin
  if fDepHandling = aValue then exit;
  fDepHandling := aValue;
  doChanged;
end;

procedure TMsgOpts.setVerbose(const aValue: boolean);
begin
  if fVerbose = aValue then exit;
  fVerbose := aValue;
  doChanged;
end;

procedure TMsgOpts.setWarnings(const aValue: boolean);
begin
  if fWarnings = aValue then exit;
  fWarnings := aValue;
  doChanged;
end;

procedure TMsgOpts.setWarnEx(const aValue: boolean);
begin
  if fWarnEx = aValue then exit;
  fWarnEx := aValue;
  doChanged;
end;

procedure TMsgOpts.setVtls(const aValue: boolean);
begin
  if fVtls = aValue then exit;
  fVtls := aValue;
  doChanged;
end;

procedure TMsgOpts.setQuiet(const aValue: boolean);
begin
  if fQuiet = aValue then exit;
  fQuiet := aValue;
  doChanged;
end;

procedure TMsgOpts.setVgc(const aValue: boolean);
begin
  if fVgc = aValue then exit;
  fVgc := aValue;
  doChanged;
end;

procedure TMsgOpts.setCol(const aValue: boolean);
begin
  if fCol = aValue then exit;
  fCol := aValue;
  doChanged;
end;
{$ENDREGION}

{$REGION TOutputOpts -----------------------------------------------------------}
constructor TOutputOpts.create;
begin
  fVerIds := TStringList.Create;
  fBoundsCheck := safeOnly;
end;

destructor TOutputOpts.destroy;
begin
  fVerIds.Free;
  inherited;
end;

procedure TOutputOpts.getOpts(aList: TStrings; base: TOptsGroup = nil);
var
  str, strbase: string;
  baseopt: TOutputOpts;
const
  trgKindStr: array[TTargetSystem] of string = ('', '-m32','-m64');
  binKindStr: array[TProjectBinaryKind] of string = ('', '-lib', '-shared', '-c');
  bchKindStr: array[TBoundCheckKind] of string = ('on', 'safeonly', 'off');
begin
  if base.isNil then
  begin
    str := binKindStr[fBinKind];
    if str <> '' then aList.Add(str);
    str := trgKindStr[fTrgKind];
    if str <> '' then aList.Add(str);
    if fUnittest then aList.Add('-unittest');
    if fInline then aList.Add('-inline');
    if fOptimz then aList.Add('-O');
    if fGenStack then aList.Add('-gs');
    if fStackStomp then aList.Add('-gx');
    if fAllInst then aList.Add('-allinst');
    if fAddMain then aList.Add('-main');
    if fRelease then aList.Add('-release');
    for str in fVerIds do
      if not isStringDisabled(str) then aList.Add('-version=' + str);
    //
    if fRelease then
      begin
        if fBoundsCheck <> safeOnly then
          aList.Add('-boundscheck=' + bchKindStr[fBoundsCheck] );
      end
    else
      if fBoundsCheck <> onAlways then
        aList.Add('-boundscheck=' + bchKindStr[fBoundsCheck] );
  end else
  begin
    baseopt := TOutputOpts(base);
    str := binKindStr[fBinKind];
    strbase := binKindStr[baseopt.fBinKind];
    if (str <> strbase) then aList.Add(str) else aList.Add(strbase);
    str := trgKindStr[fTrgKind];
    strbase := trgKindStr[baseopt.fTrgKind];
    if (str <> strbase) then aList.Add(str) else aList.Add(strbase);
    if baseopt.fUnittest or fUnittest then aList.Add('-unittest');
    if baseopt.fInline or fInline then aList.Add('-inline');
    if baseopt.fOptimz or fOptimz then aList.Add('-O');
    if baseopt.fGenStack or fGenStack then aList.Add('-gs');
    if baseopt.fStackStomp or fStackStomp then aList.Add('-gx');
    if baseopt.fAllInst or fAllInst then aList.Add('-allinst');
    if baseopt.fAddMain or fAddMain then aList.Add('-main');
    if baseopt.fRelease or fRelease then aList.Add('-release');
    if (fVerIds.Count = 0) then for str in baseopt.fVerIds do begin
      if not isStringDisabled(str) then aList.Add('-version=' + str);
    end else for str in fVerIds do
      if not isStringDisabled(str) then aList.Add('-version=' + str);
    // default values are not handled here, TODO
    if fBoundsCheck <> baseopt.fBoundsCheck then
      aList.Add('-boundscheck=' + bchKindStr[fBoundsCheck] )
    else
      aList.Add('-boundscheck=' + bchKindStr[baseopt.fBoundsCheck] );
  end;
end;

procedure TOutputOpts.assign(aValue: TPersistent);
var
  src: TOutputOpts;
begin
  if (aValue is TOutputOpts) then
  begin
    src := TOutputOpts(aValue);
    //
    fVerIds.Assign(src.fVerIds);
    fBinKind    := src.fBinKind;
    fTrgKind    := src.fTrgKind;
    fUnittest   := src.fUnittest;
    fInline     := src.fInline;
    fBoundsCheck:= src.fBoundsCheck;
    fOptimz     := src.fOptimz;
    fGenStack   := src.fGenStack;
    fAddMain    := src.fAddMain;
    fRelease    := src.fRelease;
    fAllinst    := src.fAllInst;
    fStackStomp := src.fStackStomp;
    fAlwayLinkLibs := src.fAlwayLinkLibs;
  end
  else inherited;
end;

procedure TOutputOpts.setUnittest(const aValue: boolean);
begin
  if fUnittest = aValue then exit;
  fUnittest := aValue;
  doChanged;
end;

procedure TOutputOpts.setAllInst(const aValue: boolean);
begin
  if fAllinst = aValue then exit;
  fAllinst := aValue;
  doChanged;
end;

procedure TOutputOpts.setAlwaysLinkLibs(const aValue: boolean);
begin
  if fAlwayLinkLibs = aValue then exit;
  fAlwayLinkLibs := aValue;
  doChanged;
end;

procedure TOutputOpts.setVerIds(const aValue: TStringList);
begin
  fVerIds.Assign(aValue);
  doChanged;
end;

procedure TOutputOpts.setTrgKind(const aValue: TTargetSystem);
begin
  if fTrgKind = aValue then exit;
  fTrgKind := aValue;
  doChanged;
end;

procedure TOutputOpts.setBinKind(const aValue: TProjectBinaryKind);
begin
  if fBinKind = aValue then exit;
  fBinKind := aValue;
  doChanged;
end;

procedure TOutputOpts.setInline(const aValue: boolean);
begin
  if fInline = aValue then exit;
  fInline := aValue;
  doChanged;
end;

procedure TOutputOpts.setBoundsCheck(const aValue: TBoundCheckKind);
begin
  if fBoundsCheck = aValue then exit;
  fBoundsCheck := aValue;
  doChanged;
end;

procedure TOutputOpts.setOptims(const aValue: boolean);
begin
  if fOptimz = aValue then exit;
  fOptimz := aValue;
  doChanged;
end;

procedure TOutputOpts.setGenStack(const aValue: boolean);
begin
  if fGenStack = aValue then exit;
  fGenStack := aValue;
  doChanged;
end;

procedure TOutputOpts.setAddMain(const aValue: boolean);
begin
  if fAddMain = aValue then exit;
  fAddMain := aValue;
  doChanged;
end;

procedure TOutputOpts.setRelease(const aValue: boolean);
begin
  if fRelease = aValue then exit;
  fRelease := aValue;
  doChanged;
end;

procedure TOutputOpts.setStackStomp(const aValue: boolean);
begin
  if fStackStomp = aValue then exit;
  fStackStomp := aValue;
  doChanged;
end;
{$ENDREGION}

{$REGION TDebugOpts ------------------------------------------------------------}
constructor TDebugOpts.create;
begin
  fDbgIdents := TStringList.Create;
end;

destructor TDebugOpts.destroy;
begin
  fDbgIdents.Free;
  inherited;
end;

procedure TDebugOpts.getOpts(aList: TStrings; base: TOptsGroup = nil);
var
  idt: string;
  baseopt: TDebugOpts;
begin
  if base.isNil then
  begin
    if fDebug then aList.Add('-debug');
    if fDbgLevel <> 0 then
      aList.Add('-debug=' + intToStr(fDbgLevel));
    for idt in fDbgIdents do
      aList.Add('-debug=' + idt);
    if fDbgD then aList.Add('-g');
    if fDbgC then aList.Add('-gc');
    if fGenMap then aList.Add('-map');
  end else
  begin
    baseopt := TDebugOpts(base);
    if baseopt.fDebug or fDebug then aList.Add('-debug');
    if (baseopt.fDbgLevel <> 0) and (fDbgLevel = 0) then
      aList.Add('-debug=' + intToStr(baseopt.fDbgLevel))
    else if fDbgLevel <> 0 then
      aList.Add('-debug=' + intToStr(fDbgLevel));
    if fDbgIdents.Count = 0 then
      for idt in baseopt.fDbgIdents do aList.Add('-debug=' + idt)
    else for idt in fDbgIdents do aList.Add('-debug=' + idt);
    if baseopt.fDbgD or fDbgD then aList.Add('-g');
    if baseopt.fDbgC or fDbgC then aList.Add('-gc');
    if baseopt.fGenMap or fGenMap then aList.Add('-map');
  end;
end;

procedure TDebugOpts.assign(aValue: TPersistent);
var
  src: TDebugOpts;
begin
  if (aValue is TDebugOpts) then
  begin
    src := TDebugOpts(aValue);
    //
    fDbgIdents.Assign(src.fDbgIdents);
    fDebug    := src.fDebug;
    fDbgLevel := src.fDbgLevel;
    fDbgD     := src.fDbgD;
    fDbgC     := src.fDbgC;
    fGenMap   := src.fGenMap;
  end
  else inherited;
end;

procedure TDebugOpts.updateForceDbgBool;
begin
  fForceDbgBool := (fDbgLevel > 0) or (fDbgIdents.Count > 0);
  if fForceDbgBool then setDebug(true);
end;

procedure TDebugOpts.setDebug(const aValue: boolean);
begin
  if fForceDbgBool then
  begin
    fDebug := true;
    exit;
  end;
  if fDebug = aValue then exit;
  fDebug := aValue;
  doChanged;
end;

procedure TDebugOpts.setDbgD(const aValue: boolean);
begin
  if fDbgD = aValue then exit;
  fDbgD := aValue;
  doChanged;
end;

procedure TDebugOpts.setDbgC(const aValue: boolean);
begin
  if fDbgC = aValue then exit;
  fDbgC := aValue;
  doChanged;
end;

procedure TDebugOpts.setGenMap(const aValue: boolean);
begin
  if fGenMap = aValue then exit;
  fGenMap := aValue;
  doChanged;
end;

procedure TDebugOpts.setDbgLevel(const aValue: Integer);
begin
  if fDbgLevel = aValue then exit;
  fDbgLevel := aValue;
  if fDbgLevel < 0 then fDbgLevel := 0;
  updateForceDbgBool;
  doChanged;
end;

procedure TDebugOpts.setDbgIdents(aValue: TStringList);
begin
  fDbgIdents.Assign(aValue);
  updateForceDbgBool;
  doChanged;
end;
{$ENDREGION}

{$REGION TPathsOpts ------------------------------------------------------------}
constructor TPathsOpts.create;
begin
  fExtraSrcs := TStringList.Create;
  fImpMod := TStringList.Create;
  fImpStr := TStringList.Create;
  fExcl := TStringList.Create;
  // setSrcs(), setIncl(), etc are not called when reloading from
  // a stream but rather the TSgringList.Assign()
  fExtraSrcs.OnChange := @strLstChange;
  fImpMod.OnChange := @strLstChange;
  fImpStr.OnChange := @strLstChange;
  fExcl.OnChange := @strLstChange;
end;

procedure TPathsOpts.strLstChange(sender: TObject);
begin
  TStringList(sender).BeginUpdate; // onChange not called anymore
  patchPlateformPaths(TStringList(sender));
  // EndUpdate is not called to avoid an infinite loop
end;

procedure TPathsOpts.getOpts(aList: TStrings; base: TOptsGroup = nil);
var
  str, sym: string;
  exts: TStringList;
  baseopt: TPathsOpts;
  rightList: TStringList;
begin
  if base.isNil then
  begin
    exts := TStringList.Create;
    try
      exts.AddStrings(['.d', '.di', '.dd']);
      for str in fExtraSrcs do
      begin
        if isStringDisabled(str) then
          continue;
        sym := symbolExpander.get(str);
        if not listAsteriskPath(sym, aList, exts) then
          aList.Add(sym);
      end;
    finally
      exts.Free;
    end;
    for str in fImpMod do if not isStringDisabled(str) then
      aList.Add('-I'+ symbolExpander.get(str));
    for str in fImpStr do if not isStringDisabled(str) then
      aList.Add('-J'+ symbolExpander.get(str));
    if fFname <> '' then
      aList.Add('-of' + symbolExpander.get(fFname));
    if fObjDir <> '' then
      aList.Add('-od' + symbolExpander.get(fObjDir));
  end else
  begin
    baseopt := TPathsOpts(base);
    if fExtraSrcs.Count = 0 then rightList := baseopt.fExtraSrcs
    else rightList := fExtraSrcs;
    exts := TStringList.Create;
    try
      exts.AddStrings(['.d', '.di', '.dd']);
      for str in rightList do
      begin
        if isStringDisabled(str) then
          continue;
        sym := symbolExpander.get(str);
        if not listAsteriskPath(sym, aList, exts) then
          aList.Add(sym);
      end;
    finally
      exts.Free;
    end;
    //
    if fImpMod.Count = 0 then rightList := baseopt.fImpMod
    else rightList := fImpMod;
    for str in rightList do if not isStringDisabled(str) then
      aList.Add('-I'+ symbolExpander.get(str));
    //
    if fImpStr.Count = 0 then rightList := baseopt.fImpStr
    else rightList := fImpStr;
    for str in rightList do if not isStringDisabled(str) then
      aList.Add('-J'+ symbolExpander.get(str));
    //
    str := '';
    if fFname <> '' then str := fFname else
      if baseopt.fFname <> '' then str := baseopt.fFname;
    if str <> '' then aList.Add('-of' + symbolExpander.get(str));
    //
    str := '';
    if fObjDir <> '' then str := fObjDir else
      if baseopt.fObjDir <> '' then str := baseopt.fObjDir;
    if str <> '' then aList.Add('-od' + symbolExpander.get(str));
  end;
end;

procedure TPathsOpts.assign(aValue: TPersistent);
var
  src: TPathsOpts;
begin
  if (aValue is TPathsOpts) then
  begin
    src := TPathsOpts(aValue);
    //
    fExtraSrcs.Assign(src.fExtraSrcs);
    fImpMod.Assign(src.fImpMod);
    fImpStr.Assign(src.fImpStr);
    fExcl.Assign(src.fExcl);
    fFName  := patchPlateformPath(src.fFname);
    fObjDir := patchPlateformPath(src.fObjDir);
  end
  else inherited;
end;

destructor TPathsOpts.destroy;
begin
  fExtraSrcs.free;
  fImpMod.free;
  fImpStr.free;
  fExcl.free;
  inherited;
end;

procedure TPathsOpts.setFname(const aValue: TCEFilename);
begin
  if fFname = aValue then exit;
  fFname := patchPlateformPath(aValue);
  fFname := patchPlateformExt(fFname);
  doChanged;
end;

procedure TPathsOpts.setObjDir(const aValue: TCEPathname);
begin
  if fObjDir = aValue then exit;
  fObjDir := patchPlateformPath(aValue);
  doChanged;
end;

procedure TPathsOpts.setSrcs(aValue: TStringList);
begin
  fExtraSrcs.Assign(aValue);
  patchPlateformPaths(fExtraSrcs);
  doChanged;
end;

procedure TPathsOpts.setIncl(aValue: TStringList);
begin
  fImpMod.Assign(aValue);
  patchPlateformPaths(fImpMod);
  doChanged;
end;

procedure TPathsOpts.setImpt(aValue: TStringList);
begin
  fImpStr.Assign(aValue);
  patchPlateformPaths(fImpStr);
  doChanged;
end;

procedure TPathsOpts.setExcl(aValue: TStringList);
begin
  fExcl.Assign(aValue);
  patchPlateformPaths(fExcl);
  doChanged;
end;
{$ENDREGION}

{$REGION TOtherOpts ------------------------------------------------------------}
constructor TOtherOpts.create;
begin
  fCustom := TStringList.Create;
end;

procedure TOtherOpts.assign(aValue: TPersistent);
var
  src: TOtherOpts;
begin
  if (aValue is TOtherOpts) then
  begin
    src := TOtherOpts(aValue);
    fCustom.Assign(src.fCustom);
  end
  else inherited;
end;

destructor TOtherOpts.destroy;
begin
  fCustom.Free;
  inherited;
end;

procedure TOtherOpts.getOpts(aList: TStrings; base: TOptsGroup = nil);
var
  str1, str2: string;
  baseopt: TOtherOpts;
  rightList: TStringList;
begin
  if base.isNil then
    begin
    for str1 in fCustom do if str1 <> '' then
    begin
      if isStringDisabled(str1) then
        continue;
      if str1[1] <> '-' then
        str2 := '-' + str1
      else
        str2 := str1;
      aList.AddText(symbolExpander.get(str2));
    end;
  end else
  begin
    baseopt := TOtherOpts(base);
    if fCustom.Count = 0 then rightList := baseopt.fCustom
    else rightList := fCustom;
    for str1 in rightList do if str1 <> '' then
    begin
      if isStringDisabled(str1) then
        continue;
      if str1[1] <> '-' then
        str2 := '-' + str1
      else
        str2 := str1;
      aList.AddText(symbolExpander.get(str2));
    end;
  end;
end;

procedure TOtherOpts.setCustom(aValue: TStringList);
begin
  fCustom.Assign(aValue);
  doChanged;
end;
{$ENDREGION}

{$REGION TCustomProcOptions ----------------------------------------------------}
constructor TCustomProcOptions.create;
begin
  fParameters := TStringList.Create;
end;

destructor TCustomProcOptions.destroy;
begin
  fParameters.Free;
  inherited;
end;

procedure TCustomProcOptions.assign(source: TPersistent);
var
  src: TCustomProcOptions;
begin
  if source is TCustomProcOptions then
  begin
    src := TCustomProcOptions(source);
    //
    Parameters.Assign(src.Parameters);
    fOptions    := src.fOptions;
    fExecutable := src.fExecutable;
    fShowWin    := src.fShowWin;
  end
  else inherited;
end;

procedure TCustomProcOptions.getOpts(aList: TStrings; base: TOptsGroup = nil);
begin
end;

procedure TCustomProcOptions.setProcess(var aProcess: TProcess);
begin
  //TODO-cNativeProjects: adapt TCustomProcOptions.setProcess to base/override system
  aProcess.Parameters.Clear;
  aProcess.Parameters.AddText(symbolExpander.get(Parameters.Text));
  aProcess.Executable := fExecutable;
  aProcess.ShowWindow := fShowWin;
  aProcess.Options    := fOptions;
  aProcess.CurrentDirectory := fWorkDir;
  aProcess.StartupOptions := aProcess.StartupOptions + [suoUseShowWindow];
end;

procedure TCustomProcOptions.setProcess(var aProcess: TAsyncProcess);
begin
  aProcess.Parameters.Clear;
  aProcess.Parameters.AddText(symbolExpander.get(Parameters.Text));
  aProcess.Executable := fExecutable;
  aProcess.ShowWindow := fShowWin;
  aProcess.Options    := fOptions;
  aProcess.CurrentDirectory := fWorkDir;
  aProcess.StartupOptions := aProcess.StartupOptions + [suoUseShowWindow];
end;

procedure TCustomProcOptions.setProcess(var aProcess: TCheckedAsyncProcess);
begin
  aProcess.Parameters.Clear;
  aProcess.Parameters.AddText(symbolExpander.get(Parameters.Text));
  aProcess.Executable := fExecutable;
  aProcess.ShowWindow := fShowWin;
  aProcess.Options    := fOptions;
  aProcess.CurrentDirectory := fWorkDir;
  aProcess.StartupOptions := aProcess.StartupOptions + [suoUseShowWindow];
end;

procedure TCustomProcOptions.setProcess(var aProcess: TCEProcess);
begin
  aProcess.Parameters.Clear;
  aProcess.Parameters.AddText(symbolExpander.get(Parameters.Text));
  aProcess.Executable := fExecutable;
  aProcess.ShowWindow := fShowWin;
  aProcess.Options    := fOptions;
  aProcess.CurrentDirectory := fWorkDir;
  aProcess.StartupOptions := aProcess.StartupOptions + [suoUseShowWindow];
end;

procedure TCustomProcOptions.setExecutable(const aValue: TCEFilename);
begin
  if fExecutable = aValue then exit;
  fExecutable := aValue;
  doChanged;
end;

procedure TCustomProcOptions.setWorkDir(const aValue: TCEPathname);
begin
  if fWorkDir = aValue then exit;
  fWorkDir := aValue;
  doChanged;
end;

procedure TCustomProcOptions.setOptions(const aValue: TProcessOptions);
begin
  if fOptions = aValue then exit;
  fOptions := aValue;
  doChanged;
end;

procedure TCustomProcOptions.setParameters(aValue: TStringList);
begin
  fParameters.Assign(aValue);
  doChanged;
end;

procedure TCustomProcOptions.setShowWin(const aValue: TShowWindowOptions);
begin
  if fShowWin = aValue then exit;
  fShowWin := aValue;
  doChanged;
end;
{$ENDREGION}

{$REGION TCompilerConfiguration ------------------------------------------------}
constructor TCompilerConfiguration.create(aCollection: TCollection);
begin
  inherited create(aCollection);

  fDocOpts    := TDocOpts.create;
  fDebugOpts  := TDebugOpts.create;
  fMsgOpts    := TMsgOpts.create;
  fOutputOpts := TOutputOpts.create;
  fPathsOpts  := TPathsOpts.create;
  fOthers     := TOtherOpts.create;
  fPreProcOpt := TCompileProcOptions.create;
  fPostProcOpt:= TCompileProcOptions.create;
  fRunProjOpt := TProjectRunOptions.create;

  fDocOpts.onChange     := @subOptsChanged;
  fDebugOpts.onChange   := @subOptsChanged;
  fMsgOpts.onChange     := @subOptsChanged;
  fOutputOpts.onChange  := @subOptsChanged;
  fPathsOpts.onChange   := @subOptsChanged;
  fOthers.onChange      := @subOptsChanged;
  fPreProcOpt.onChange  := @subOptsChanged;
  fPostProcOpt.onChange := @subOptsChanged;
  fRunProjOpt.onChange  := @subOptsChanged;

  fName := nameFromID;
end;

destructor TCompilerConfiguration.destroy;
begin
  fOnChanged := nil;
  fDocOpts.free;
  fDebugOpts.free;
  fMsgOpts.free;
  fOutputOpts.free;
  fPathsOpts.free;
  fOthers.free;
  fPreProcOpt.free;
  fPostProcOpt.free;
  fRunProjOpt.Free;
  inherited;
end;

procedure TCompilerConfiguration.assign(aValue: TPersistent);
var
  src: TCompilerConfiguration;
begin
  if (aValue is TCompilerConfiguration) then
  begin
    src := TCompilerConfiguration(aValue);
    //
    fDocOpts.assign(src.fDocOpts);
    fDebugOpts.assign(src.fDebugOpts);
    fMsgOpts.assign(src.fMsgOpts);
    fOutputOpts.assign(src.fOutputOpts);
    fPathsOpts.assign(src.fPathsOpts);
    fOthers.assign(src.fOthers);
    fPreProcOpt.assign(src.fPreProcOpt);
    fPostProcOpt.assign(src.fPostProcOpt);
    fRunProjOpt.assign(src.fRunProjOpt);
    //
    // isBase / isOverriden not copied by purpose.
  end
  else inherited;
end;

function TCompilerConfiguration.nameFromID: string;
begin
  result := format('<configuration %d>', [ID]);
end;

procedure TCompilerConfiguration.getOpts(aList: TStrings; base: TCompilerConfiguration = nil);
begin
  if (base = nil) or (base = self) then
  begin
    fDocOpts.getOpts(aList);
    fDebugOpts.getOpts(aList);
    fMsgOpts.getOpts(aList);
    fOutputOpts.getOpts(aList);
    fPathsOpts.getOpts(aList);
    fOthers.getOpts(aList);
  end else
  begin
    fDocOpts.getOpts(aList, base.fDocOpts);
    fDebugOpts.getOpts(aList, base.fDebugOpts);
    fMsgOpts.getOpts(aList, base.fMsgOpts);
    fOutputOpts.getOpts(aList, base.fOutputOpts);
    fPathsOpts.getOpts(aList, base.fPathsOpts);
    fOthers.getOpts(aList, base.fOthers);
  end;
end;

procedure TCompilerConfiguration.setName(const aValue: string);
begin
  if fName = aValue then
    exit;
  fName := aValue;
  if fName = '' then
    fName := nameFromID;
  doChanged;
end;

procedure TCompilerConfiguration.subOptsChanged(sender: TObject);
begin
  doChanged;
end;

procedure TCompilerConfiguration.doChanged;
begin
  if assigned(fOnChanged) then fOnChanged(self);
end;

procedure TCompilerConfiguration.setDocOpts(const aValue: TDocOpts);
begin
  fDocOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setDebugOpts(const aValue: TDebugOpts);
begin
  fDebugOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setMsgOpts(const aValue: TMsgOpts);
begin
  fMsgOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setOutputOpts(const aValue: TOutputOpts);
begin
  fOutputOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setPathsOpts(const aValue: TPathsOpts);
begin
  fPathsOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setOthers(const aValue: TOtherOpts);
begin
  fOthers.Assign(aValue);
end;

procedure TCompilerConfiguration.setPreProcOpt(const aValue: TCompileProcOptions);
begin
  fPreProcOpt.assign(aValue);
end;

procedure TCompilerConfiguration.setPostProcOpt(const aValue: TCompileProcOptions);
begin
  fPostProcOpt.assign(aValue);
end;

procedure TCompilerConfiguration.setRunProjOpt(const aValue: TProjectRunOptions);
begin
  fRunProjOpt.assign(aValue);
end;

procedure TCompilerConfiguration.setisBaseConfiguration(const aValue: boolean);
begin
  fIsBaseConfiguration := aValue;
  doChanged;
end;

procedure TCompilerConfiguration.setisOverriddenConfiguration(const aValue: boolean);
begin
  fIsBaseConfiguration := false;
  fIsOverriddenConfiguration := aValue;
  doChanged;
end;
{$ENDREGION}

initialization
  RegisterClasses([TOtherOpts, TPathsOpts, TDebugOpts, TOutputOpts, TMsgOpts,
    TDocOpts, TCompileProcOptions, TProjectRunOptions, TCompilerConfiguration]);
end.
