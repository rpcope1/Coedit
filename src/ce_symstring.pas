unit ce_symstring;

{$I ce_defines.inc}

interface

uses
  ce_observer, ce_interfaces, ce_project, ce_synmemo, ce_common;

type

  (**
   * Enumerates the symbol kinds, used to index an associative array.
   *)
  TCESymbol = (CAF, CAP, CFF, CFP, CI, CPF, CPP, CPO, CPR, CPN, CPFS);

  (**
   * TCESymbolExpander is designed to expand Coedit symbolic strings,
   * using the information collected from several observer interfaces.
   *)
  TCESymbolExpander = class(ICEMultiDocObserver, ICEProjectObserver)
  private
    fProj: TCEProject;
    fDoc: TCESynMemo;
    fSymbols: array[TCESymbol] of string;
    procedure updateSymbols;
    //
    procedure projNew(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
    procedure projCompiling(aProject: TCEProject);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
  public
    constructor create;
    destructor destroy; override;
    // expands the symbols contained in symString
    function get(const symString: string): string;
  end;

var
  symbolExpander: TCESymbolExpander;

implementation

uses
  Forms, sysutils, classes;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCESymbolExpander.create;
begin
  EntitiesConnector.addObserver(self);
end;

destructor TCESymbolExpander.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCESymbolExpander.projNew(aProject: TCEProject);
begin
  fProj := aProject;
end;

procedure TCESymbolExpander.projClosing(aProject: TCEProject);
begin
  if fProj <> aProject then exit;
  fProj := nil;
end;

procedure TCESymbolExpander.projFocused(aProject: TCEProject);
begin
  fProj := aProject;
end;

procedure TCESymbolExpander.projChanged(aProject: TCEProject);
begin
  if fProj <> aProject then exit;
end;

procedure TCESymbolExpander.projCompiling(aProject: TCEProject);
begin
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCESymbolExpander.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCESymbolExpander.docClosing(aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then exit;
  fDoc := nil;
end;

procedure TCESymbolExpander.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCESymbolExpander.docChanged(aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then exit;
end;
{$ENDREGION}

{$REGION Symbol things ---------------------------------------------------------}
procedure TCESymbolExpander.updateSymbols;
var
  hasProj: boolean;
  hasDoc: boolean;
  i: Integer;
const
  na = '``';
begin
  hasProj := fProj <> nil;
  hasDoc  := fDoc <> nil;
  // application
  fSymbols[CAF] := Application.ExeName;
  fSymbols[CAP] := ExtractFilePath(Application.ExeName);
  // document
  if hasDoc then
  begin
    if fileExists(fDoc.fileName) then begin
      fSymbols[CFF] := fDoc.fileName;
      fSymbols[CFP] := ExtractFilePath(fDoc.fileName);
    end
    else begin
      fSymbols[CFF] := na;
      fSymbols[CFP] := na;
    end;
    if fDoc.Identifier <> '' then
      fSymbols[CI] := fDoc.Identifier
    else fSymbols[CI] := na;
  end else begin
    fSymbols[CFF] := na;
    fSymbols[CFP] := na;
    fSymbols[CI ] := na;
  end;
  // project
  if hasProj then
  begin
    if fileExists(fProj.fileName) then begin
      fSymbols[CPF] := fProj.fileName;
      fSymbols[CPP] := ExtractFilePath(fProj.fileName);
      fSymbols[CPR] := fProj.getAbsoluteFilename(fProj.RootFolder);
      fSymbols[CPN] := stripFileExt(extractFileName(fProj.fileName));
      fSymbols[CPO] := fProj.outputFilename;
      if fSymbols[CPR] = '' then fSymbols[CPR] := fSymbols[CPP];
    end else begin
      fSymbols[CPF] := na;
      fSymbols[CPP] := na;
      fSymbols[CPR] := na;
      fSymbols[CPN] := na;
      fSymbols[CPO] := na;
    end;
    fSymbols[CPFS] := '';
    for i := 0 to fProj.Sources.Count-1 do
    begin
      fSymbols[CPFS] += fProj.getAbsoluteSourceName(i);
      if fProj.Sources.Count > 1 then
        if i <> fProj.Sources.Count-1 then
          fSymbols[CPFS] += LineEnding;
    end;
    if fProj.Sources.Count = 0 then fSymbols[CPFS] := na;
  end else begin
    fSymbols[CPF] := na;
    fSymbols[CPP] := na;
    fSymbols[CPR] := na;
    fSymbols[CPN] := na;
    fSymbols[CPO] := na;
    fSymbols[CPFS]:= na;
  end;
end;

function TCESymbolExpander.get(const symString: string): string;
var
  elems: TStringList;
  elem: string;
  begs, ends: boolean;
  i: integer;
begin
  result := '';
  if symString = '' then exit;
  updateSymbols;
  //
  elems := TStringList.Create;
  try
    i := 0;
    elem := '';
    repeat
      inc(i);
      if not (symString[i] in ['<', '>']) then
        elem += symString[i]
      else
      begin
        if symString[i] = '<' then
          begs := true;
        ends := symString[i] = '>';
        elems.Add(elem);
        elem := '';
        if begs and ends then
        begin
          begs := false;
          ends := false;
          // elem.obj is a flag to diferenciate symbols from elements
          elems.Objects[elems.Count-1] := Self;
        end;
      end;
    until
      i = length(symString);
    elems.Add(elem);
    elem := '';
    for i:= 0 to elems.Count-1 do
    begin
      if elems.Objects[i] = nil then
        result += elems.Strings[i]
      else case elems.Strings[i] of
        '<','>': continue;
        'CAF',  'CoeditApplicationFile':result += fSymbols[CAF];
        'CAP',  'CoeditApplicationPath':result += fSymbols[CAP];
        //
        'CFF',  'CurrentFileFile':      result += fSymbols[CFF];
        'CFP',  'CurrentFilePath':      result += fSymbols[CFP];
        'CI',   'CurrentIdentifier':    result += fSymbols[CI];
        //
        'CPF',  'CurrentProjectFile':   result += fSymbols[CPF];
        'CPFS', 'CurrentProjectFiles':  result += fSymbols[CPFS];
        'CPN',  'CurrentProjectName':   result += fSymbols[CPN];
        'CPO',  'CurrentProjectOutput': result += fSymbols[CPO];
        'CPP',  'CurrentProjectPath':   result += fSymbols[CPP];
        'CPR',  'CurrentProjectRoot':   result += fSymbols[CPR];
      end;
    end;
  finally
    elems.Free;
  end;
end;
{$ENDREGION}

initialization
  symbolExpander := TCESymbolExpander.create;
finalization
  symbolExpander.Free;
end.

