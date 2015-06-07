unit ce_symstring;

{$I ce_defines.inc}

interface

uses
  ce_observer, ce_interfaces, ce_project, ce_synmemo, ce_common;

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
    fProj: TCEProject;
    fDoc: TCESynMemo;
    fNeedUpdate: boolean;
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
procedure TCESymbolExpander.projNew(aProject: TCEProject);
begin
  fProj := aProject;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.projClosing(aProject: TCEProject);
begin
  if fProj <> aProject then
    exit;
  fProj := nil;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.projFocused(aProject: TCEProject);
begin
  fProj := aProject;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.projChanged(aProject: TCEProject);
begin
  if fProj <> aProject then
    exit;
  fNeedUpdate := true;
end;

procedure TCESymbolExpander.projCompiling(aProject: TCEProject);
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
  hasProj: boolean;
  hasDoc: boolean;
  fname: string;
  i: Integer;
  str: TStringList;
const
  na = '``';
begin
  if not fNeedUpdate then exit;
  fNeedUpdate := false;
  hasProj := fProj <> nil;
  hasDoc := fDoc <> nil;
  // application
  fSymbols[CAF] := Application.ExeName;
  fSymbols[CAP] := ExtractFilePath(Application.ExeName);
  // document
  if hasDoc then
  begin
    if fileExists(fDoc.fileName) then
    begin
      fSymbols[CFF] := fDoc.fileName;
      fSymbols[CFP] := ExtractFilePath(fDoc.fileName);
    end
    else
    begin
      fSymbols[CFF] := na;
      fSymbols[CFP] := na;
    end;
    if fDoc.Identifier <> '' then
      fSymbols[CI] := fDoc.Identifier
    else
      fSymbols[CI] := na;
  end
  else
  begin
    fSymbols[CFF] := na;
    fSymbols[CFP] := na;
    fSymbols[CI] := na;
  end;
  // project
  if hasProj then
  begin
    if fileExists(fProj.fileName) then
    begin
      fSymbols[CPF] := fProj.fileName;
      fSymbols[CPP] := ExtractFilePath(fProj.fileName);
      fSymbols[CPR] := fProj.getAbsoluteFilename(fProj.RootFolder);
      fSymbols[CPN] := stripFileExt(extractFileName(fProj.fileName));
      fSymbols[CPO] := fProj.outputFilename;
      if fSymbols[CPR] = '' then
        fSymbols[CPR] := fSymbols[CPP];
    end
    else
    begin
      fSymbols[CPF] := na;
      fSymbols[CPP] := na;
      fSymbols[CPR] := na;
      fSymbols[CPN] := na;
      fSymbols[CPO] := na;
    end;
    if fProj.Sources.Count = 0 then
    begin
      fSymbols[CPFS] := na;
      fSymbols[CPCD] := na;
    end
    else
    begin
      str := TStringList.Create;
      try
        for i := 0 to fProj.Sources.Count-1 do
        begin
          fname := fProj.getAbsoluteSourceName(i);
          if dExtList.IndexOf(ExtractFileExt(fname)) = -1 then
            continue;
          str.Add(fname);
        end;
        fSymbols[CPFS] := str.Text;
        fSymbols[CPCD] := commonFolder(str);
      finally
        str.Free;
      end;
    end;
  end
  else
  begin
    fSymbols[CPF] := na;
    fSymbols[CPP] := na;
    fSymbols[CPR] := na;
    fSymbols[CPN] := na;
    fSymbols[CPO] := na;
    fSymbols[CPFS] := na;
    fSymbols[CPCD] := na;
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
          'CFF', 'CurrentFileFile': Result += fSymbols[CFF];
          'CFP', 'CurrentFilePath': Result += fSymbols[CFP];
          'CI', 'CurrentIdentifier': Result += fSymbols[CI];
          //
          'CPF', 'CurrentProjectFile': Result += fSymbols[CPF];
          'CPFS', 'CurrentProjectFiles': Result += fSymbols[CPFS];
          'CPN', 'CurrentProjectName': Result += fSymbols[CPN];
          'CPO', 'CurrentProjectOutput': Result += fSymbols[CPO];
          'CPP', 'CurrentProjectPath': Result += fSymbols[CPP];
          'CPR', 'CurrentProjectRoot': Result += fSymbols[CPR];
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
