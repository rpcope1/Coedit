unit ce_symstring;

{$I ce_defines.inc}

interface

uses
  ce_observer, ce_interfaces, ce_nativeproject, ce_synmemo, ce_common;

type

  (**
   * Enumerates the symbol kinds, used to index an associative array.
   *)
  TCESymbol = (CAF, CAP, CFF, CFP, CI, CPF, CPP, CPO, CPR, CPN, CPFS, CPCD);

  (**
   * TCESymbolExpander is designed to expand Coedit symbolic strings,
   * using the information collected from several observer interfaces.
   *)
  TCESymbolExpander = class(ICEMultiDocObserver, ICEProjectObserver)
  private
    fProj: TCENativeProject;
    fProjInterface: ICECommonProject;
    fDoc: TCESynMemo;
    fNeedUpdate: boolean;
    fSymbols: array[TCESymbol] of string;
    procedure updateSymbols;
    //
    procedure projNew(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
  public
    constructor Create;
    destructor Destroy; override;
    // expands the symbols contained in symString
    function get(const symString: string): string;
  end;

var
  symbolExpander: TCESymbolExpander;

implementation

uses
  Forms, SysUtils, Classes;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCESymbolExpander.Create;
begin
  EntitiesConnector.addObserver(self);
  fNeedUpdate := true;
end;

destructor TCESymbolExpander.Destroy;
begin
  fNeedUpdate := false;
  EntitiesConnector.removeObserver(self);
  inherited;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCESymbolExpander.projNew(aProject: ICECommonProject);
begin
  fProjInterface := aProject;
  case aProject.getFormat of
    pfNative: fProj := TCENativeProject(aProject.getProject);
    pfDub: fProj := nil;
  end;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.projClosing(aProject: ICECommonProject);
begin
  fProjInterface := nil;
  if fProj <> aProject.getProject then
    exit;
  fProj := nil;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.projFocused(aProject: ICECommonProject);
begin
  fProjInterface := aProject;
  case aProject.getFormat of
    pfNative: fProj := TCENativeProject(aProject.getProject);
    pfDub: fProj := nil;
  end;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.projChanged(aProject: ICECommonProject);
begin
  fProjInterface := aProject;
  if fProj <> aProject.getProject then
    exit;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.projCompiling(aProject: ICECommonProject);
begin
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCESymbolExpander.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.docClosing(aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then
    exit;
  fDoc := nil;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.docFocused(aDoc: TCESynMemo);
begin
  if (aDoc <> nil) and (fDoc = aDoc) then
    exit;
  fDoc := aDoc;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.docChanged(aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then
    exit;
  fNeedUpdate := true;
end;

{$ENDREGION}

{$REGION Symbol things ---------------------------------------------------------}
procedure TCESymbolExpander.updateSymbols;
var
  hasNativeProj: boolean;
  hasProjItf: boolean;
  hasDoc: boolean;
  fname: string;
  i: Integer;
  e: TCESymbol;
  str: TStringList;
const
  na = '``';
begin
  if not fNeedUpdate then exit;
  fNeedUpdate := false;
  //
  hasNativeProj := fProj <> nil;
  hasProjItf := fProjInterface <> nil;
  hasDoc := fDoc <> nil;
  //
  for e := low(TCESymbol) to high(TCESymbol) do
    fSymbols[e] := na;
  //
  // application
  fSymbols[CAF] := Application.ExeName;
  fSymbols[CAP] := ExtractFilePath(fSymbols[CAF]);
  // document
  if hasDoc then
  begin
    if not fileExists(fDoc.fileName) then
      fDoc.saveTempFile;
    fSymbols[CFF] := fDoc.fileName;
    fSymbols[CFP] := ExtractFilePath(fDoc.fileName);
    if fDoc.Identifier <> '' then
      fSymbols[CI] := fDoc.Identifier;
  end;
  // project interface
  if hasProjItf then
  begin
    fSymbols[CPF] := fProjInterface.getFilename;
    fSymbols[CPP] := ExtractFilePath(fSymbols[CPF]);
    fSymbols[CPN] := stripFileExt(extractFileName(fSymbols[CPF]));
  end;
  // TODO-cDUB: move to upper block expansion of CPO, CPFS & CPCD when implemented in ICECOmmonProject
  if hasNativeProj then
  begin
    if fileExists(fProj.fileName) then
    begin
      fSymbols[CPR] := fProj.getAbsoluteFilename(fProj.RootFolder);
      fSymbols[CPO] := fProj.getOutputFilename;
      if fSymbols[CPR] = '' then
        fSymbols[CPR] := fSymbols[CPP];
    end;
    if fProj.Sources.Count <> 0 then
    begin
      str := TStringList.Create;
      try
        for i := 0 to fProj.Sources.Count-1 do
        begin
          fname := fProj.getAbsoluteSourceName(i);
          if not isEditable(ExtractFileExt(fname)) then
            continue;
          str.Add(fname);
        end;
        fSymbols[CPFS] := str.Text;
        if str.Count = 1 then
          fSymbols[CPCD] := ExtractFileDir(Str.Strings[0])
        else
          fSymbols[CPCD] := commonFolder(str);
      finally
        str.Free;
      end;
    end;
  end;
end;

function TCESymbolExpander.get(const symString: string): string;
var
  elems: TStringList;
  elem: string;
  begs, ends: boolean;
  i: integer;
begin
  Result := '';
  if symString = '' then
    exit;
  //
  updateSymbols;
  elems := TStringList.Create;
  try
    i := 0;
    elem := '';
    repeat
      Inc(i);
      if not (symString[i] in ['<', '>']) then
        elem += symString[i]
      else
      begin
        if symString[i] = '<' then
          begs := True;
        ends := symString[i] = '>';
        elems.Add(elem);
        elem := '';
        if begs and ends then
        begin
          begs := False;
          ends := False;
          // elem.obj is a flag to differenciate symbols from elements
          elems.Objects[elems.Count - 1] := Self;
        end;
      end;
    until
      i = length(symString);
    elems.Add(elem);
    elem := '';
    for i := 0 to elems.Count - 1 do
    begin
      if elems.Objects[i] = nil then
        Result += elems.Strings[i]
      else
        case elems.Strings[i] of
          '<', '>': continue;
          'CAF', 'CoeditApplicationFile': Result += fSymbols[CAF];
          'CAP', 'CoeditApplicationPath': Result += fSymbols[CAP];
          //
          'CFF', 'CurrentFileFile'    : Result += fSymbols[CFF];
          'CFP', 'CurrentFilePath'    : Result += fSymbols[CFP];
          'CI', 'CurrentIdentifier'   : Result += fSymbols[CI];
          //
          'CPF', 'CurrentProjectFile'   : Result += fSymbols[CPF];
          'CPFS', 'CurrentProjectFiles' : Result += fSymbols[CPFS];
          'CPN', 'CurrentProjectName'   : Result += fSymbols[CPN];
          'CPO', 'CurrentProjectOutput' : Result += fSymbols[CPO];
          'CPP', 'CurrentProjectPath'   : Result += fSymbols[CPP];
          'CPR', 'CurrentProjectRoot'   : Result += fSymbols[CPR];
          'CPCD','CurrentProjectCommonDirectory': Result += fSymbols[CPCD];
        end;
    end;
  finally
    elems.Free;
  end;
end;

{$ENDREGION}

initialization
  symbolExpander := TCESymbolExpander.Create;

finalization
  symbolExpander.Free;
end.
