unit ce_dfmt;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, ExtCtrls,
  Menus, Buttons, process, ce_widget, ce_interfaces, ce_observer, ce_synmemo,
  ce_writableComponent, ce_common, ce_sharedres, PropEdits, ObjectInspector;

type

  DfmtEol = (cr, lf, crlf);
  DfmtIdentstyle = (tab, space);
  DfmtBraceStyle = (allman, otbs, stroustrup);

  // wraps dfmt options to build the command line with ease
  // and allows to save the options between session.
  TCEDmtWrapper = class(TWritableLfmTextComponent)
  private
    fEol: DfmtEol;
    fTabStyle: DfmtIdentstyle;
    fIdentSize: integer;
    fTabWidth: integer;
    fHardLLen: integer;
    fSoftLLen: integer;
    fBraceStyle: DfmtBraceStyle;
    fSpaceCast: boolean;
    fSplitOp: boolean;
    fCompactLbl: boolean;
    fSpaceSelImp: boolean;
  published
    property endOfline: DfmtEol read fEol write fEol default lf;
    property identationStyle: DfmtIdentstyle read fTabStyle write fTabStyle default space;
    property identSize: integer read fIdentSize write fIdentSize default 4;
    property tabWidth: integer read fTabWidth write fTabWidth default 8;
    property hardLineLen: integer read fHardLLen write fHardLLen default 120;
    property softLineLen: integer read fSoftLLen write fSoftLLen default 80;
    property braceStyle: DfmtBraceStyle read fBraceStyle write fBraceStyle default allman;
    property spaceAfterCast: boolean read fSpaceCast write fSpaceCast default true;
    property spaceAfterImport: boolean read fSpaceSelImp write fSpaceSelImp default true;
    property splitOpAtPrevLine: boolean read fSplitOp write fSplitOp default true;
    property compactLabeledStatements: boolean read fCompactLbl write fCompactLbl default true;
  public
    constructor create(AOwner: TComponent); override;
    procedure getCommandLine(str: TStrings);
  end;

  { TCEDfmtWidget }

  TCEDfmtWidget = class(TCEWidget, ICEMultiDocObserver)
    btnApply: TSpeedButton;
    btnCancel: TSpeedButton;
    pnlFooter: TPanel;
    dfmtOptionEditor: TTIPropertyGrid;
    procedure dfmtOptionEditorEditorFilter(Sender: TObject;
      aEditor: TPropertyEditor; var aShow: boolean);
  private
    fDoc: TCESynMemo;
    fBackup: TStringList;
    fDmtWrapper: TCEDmtWrapper;
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    //
    procedure doApply(sender: TObject);
    procedure doCancel(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

const
  optFname = 'dfmt.txt';

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEDfmtWidget.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fDmtWrapper := TCEDmtWrapper.Create(self);
  fBackup := TStringList.Create;
  //
  fname := getCoeditDocPath + optFname;
  if fileExists(fname) then
    fDmtWrapper.loadFromFile(fname);
  //
  btnCancel.OnClick := @doCancel;
  btnApply.OnClick  := @doApply;
  AssignPng(btnCancel, 'cancel');
  AssignPng(btnApply, 'accept');
  //
  dfmtOptionEditor.TIObject := fDmtWrapper;
end;

destructor TCEDfmtWidget.destroy;
begin
  dfmtOptionEditor.TIObject := nil;
  fDmtWrapper.saveToFile(getCoeditDocPath + optFname);
  fBackup.Free;
  inherited;
end;

constructor TCEDmtWrapper.create(AOwner: TComponent);
begin
  inherited;
  fEol          := lf;
  fTabStyle     := DfmtIdentstyle.space;
  fIdentSize    := 4;
  fTabWidth     := 8;
  fHardLLen     := 120;
  fSoftLLen     := 80;
  fBraceStyle   := DfmtBraceStyle.allman;
  fSpaceCast    := true;
  fSpaceSelImp  := true;
  fSplitOp      := true;
  fCompactLbl   := true;
end;

procedure TCEDfmtWidget.dfmtOptionEditorEditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  case aEditor.GetName of
    'Tag', 'Name': aShow := false;
    else aShow := true;
  end;
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEDfmtWidget.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEDfmtWidget.docFocused(aDoc: TCESynMemo);
begin
  if aDoc = fDoc
    then exit;
  fDoc := aDoc;
end;

procedure TCEDfmtWidget.docChanged(aDoc: TCESynMemo);
begin
end;

procedure TCEDfmtWidget.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then
    exit;
  fDoc := nil;
end;
{$ENDREGION}

{$REGION Dfmt things -----------------------------------------------------------}
procedure TCEDmtWrapper.getCommandLine(str: TStrings);
const
  eol: array[DfmtEol] of string = ('cr', 'lf', 'crlf');
  falsetrue: array[boolean] of string = ('false', 'true');
  idtstyle: array[DfmtIdentstyle] of string = ('tab', 'space');
  brc: array[DfmtBraceStyle] of string = ('allman', 'otbs', 'stroustrup');
begin
  str.Add('--end_of_line=' + eol[endOfline]);
  str.Add('--max_line_length=' + intToStr(hardLineLen));
  str.Add('--soft_max_line_length=' + intToStr(softLineLen));
  str.Add('--indent_size='  + intToStr(identSize));
  str.Add('--indent_style=' + idtstyle[identationStyle]);
  str.Add('--tab_width=' + intToStr(tabWidth));
  str.Add('--brace_style=' + brc[braceStyle]);
  str.Add('--split_operator_at_line_end=' + falsetrue[splitOpAtPrevLine]);
  str.Add('--space_after_cast=' + falsetrue[spaceAfterCast]);
  str.Add('--selective_import_space=' + falsetrue[spaceAfterImport]);
  str.Add('--compact_labeled_statements=' + falsetrue[compactLabeledStatements]);
end;

procedure TCEDfmtWidget.doApply(sender: TObject);
var
  inp: string;
  prc: TProcess;
  str: TStringList;
begin
  if not assigned(fDoc) then
    exit;

  fBackup.Assign(fDoc.Lines);
  prc := TProcess.create(nil);
  try
    fDmtWrapper.getCommandLine(prc.Parameters);
    prc.Options:= prc.Options + [poUsePipes, poStderrToOutPut];
    prc.Executable:= exeFullName('dfmt' + exeExt);
    prc.Execute;
    inp := fDoc.Lines.Text;
    prc.Input.Write(inp[1], length(inp));
    prc.CloseInput;
    while prc.Running do (*!*);
    try
      str := TStringList.Create;
      processOutputToStrings(prc,str);
      fDoc.SelectAll;
      fDoc.SelText:= str.Text;
    except
      fDoc.Lines.Assign(fBackup);
    end;
  finally
    prc.free;
    str.free;
  end;
end;

procedure TCEDfmtWidget.doCancel(sender: TObject);
begin
  if not assigned(fDoc) then
    exit;

  fDoc.Lines.Assign(fBackup);
end;
{$ENDREGION}

end.

