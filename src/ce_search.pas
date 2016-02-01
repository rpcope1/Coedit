unit ce_search;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, actnList, Buttons, SynEdit, SynEditSearch, SynEditTypes,
  ce_common, ce_mru, ce_widget, ce_synmemo, ce_interfaces, ce_observer,
  ce_writableComponent, ce_dialogs, ce_sharedres, SynEditTextBuffer;

type

  // TCESearchWidget persistents settings
  TCESearchOptions = class(TWritableLfmTextComponent)
  private
    fPrompt: boolean;
    fFromCur: boolean;
    fRegex: boolean;
    fCaseSens:boolean;
    fBackWard: boolean;
    fWholeWord: boolean;
    fMrSearches: TStringList;
    fMrReplacements: TStringList;
    procedure setMrSearches(aValue: TStringList);
    procedure setMrReplacements(aValue: TStringList);
  published
    property prompt: boolean read fPrompt write fPrompt;
    property fromCursor: boolean read fFromCur write fFromCur;
    property regex: boolean read fRegex write fRegex;
    property caseSensistive: boolean read fCaseSens write fCaseSens;
    property backward: boolean read fBackWard write fBackWard;
    property wholeWord: boolean read fWholeWord write fWholeWord;
    property recentSearches: TStringList read fMrSearches write setMrSearches;
    property recentReplacements: TStringList read fMrReplacements write setMrReplacements;
  public
    procedure afterLoad; override;
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure Assign(aValue: TPersistent); override;
    procedure AssignTo(aValue: TPersistent); override;
  end;

  { TCESearchWidget }
  TCESearchWidget = class(TCEWidget, ICEMultiDocObserver, ICEProjectObserver)
    btnAllScope: TBitBtn;
    btnFind: TBitBtn;
    btnFindAll: TBitBtn;
    btnReplace: TBitBtn;
    btnReplaceAll: TBitBtn;
    cbToFind: TComboBox;
    cbReplaceWth: TComboBox;
    chkEnableRep: TCheckBox;
    chkPrompt: TCheckBox;
    chkRegex: TCheckBox;
    chkWWord: TCheckBox;
    chkBack: TCheckBox;
    chkFromCur: TCheckBox;
    chkCaseSens: TCheckBox;
    grpOpts: TGroupBox;
    imgList: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnAllScopeClick(Sender: TObject);
    procedure cbReplaceWthChange(Sender: TObject);
    procedure cbToFindChange(Sender: TObject);
    procedure chkEnableRepChange(Sender: TObject);
  private
    fDoc: TCESynMemo;
    fToFind: string;
    fReplaceWth: string;
    fActReplaceNext: TAction;
    fActFindNext: TAction;
    fActReplaceAll: TAction;
    fActFindAll: TAction;
    fSearchMru, fReplaceMru: TCEMruList;
    fCancelAll: boolean;
    fHasSearched: boolean;
    fHasRestarted: boolean;
    fProj: ICECommonProject;
    fAllInProj: boolean;
    function getOptions: TSynSearchOptions;
    procedure actReplaceAllExecute(sender: TObject);
    procedure replaceEvent(Sender: TObject; const ASearch, AReplace:
      string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
    //
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    procedure projCompiled(aProject: ICECommonProject; success: boolean);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    //
    procedure findAll(const filename: string; lines: TStrings);
  protected
    procedure updateImperative; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure actFindNextExecute(sender: TObject);
    procedure actReplaceNextExecute(sender: TObject);
    procedure actFindAllExecute(sender: TObject);
  end;

implementation
{$R *.lfm}

const
  OptsFname = 'search.txt';

{$REGION TCESearchOptions ------------------------------------------------------}
constructor TCESearchOptions.create(aOwner: TComponent);
begin
  inherited;
  fMrReplacements := TStringList.Create;
  fMrSearches := TStringList.Create;
end;

destructor TCESearchOptions.destroy;
begin
  fMrSearches.Free;
  fMrReplacements.Free;
  inherited;
end;

procedure TCESearchOptions.Assign(aValue: TPersistent);
var
  widg: TCESearchWidget;
begin
  if aValue is TCESearchWidget then
  begin
    widg := TCESearchWidget(aValue);
    fMrSearches.Assign(widg.fSearchMru);
    fMrReplacements.Assign(widg.fReplaceMru);
    fPrompt     := widg.chkPrompt.Checked;
    fBackWard   := widg.chkBack.Checked;
    fCaseSens   := widg.chkCaseSens.Checked;
    fRegex      := widg.chkRegex.Checked;
    fFromCur    := widg.chkFromCur.Checked;
    fWholeWord  := widg.chkWWord.Checked;
  end
  else inherited;
end;

procedure TCESearchOptions.AssignTo(aValue: TPersistent);
var
  widg: TCESearchWidget;
begin
  if aValue is TCESearchWidget then
  begin
    widg := TCESearchWidget(aValue);
    widg.cbToFind.Items.Assign(fMrSearches);
    widg.fSearchMru.Assign(fMrSearches);
    widg.cbReplaceWth.Items.Assign(fMrReplacements);
    widg.fReplaceMru.Assign(fMrReplacements);
    widg.chkPrompt.Checked  := fPrompt;
    widg.chkBack.Checked    := fBackWard;
    widg.chkCaseSens.Checked:= fCaseSens;
    widg.chkRegex.Checked   := fRegex;
    widg.chkFromCur.Checked := fFromCur;
    widg.chkWWord.Checked   := fWholeWord;
  end
  else inherited;
end;

procedure TCESearchOptions.setMrSearches(aValue: TStringList);
begin
  fMrSearches.Assign(aValue);
end;

procedure TCESearchOptions.setMrReplacements(aValue: TStringList);
begin
  fMrReplacements.Assign(aValue);
end;

procedure TCESearchOptions.afterLoad;
var
  i: integer;
begin
  inherited;
  for i := fMrReplacements.Count-1 downto 0 do
    if fMrReplacements[i].length > 128 then
      fMrReplacements.Delete(i);
  for i := fMrSearches.Count-1 downto 0 do
      if fMrSearches[i].length > 128 then
        fMrSearches.Delete(i);
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCESearchWidget.Create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fActFindNext := TAction.Create(self);
  fActFindNext.Caption := 'Find';
  fActFindNext.OnExecute := @actFindNextExecute;
  fActFindAll := TAction.Create(self);
  fActFindAll.Caption := 'Find all';
  fActFindAll.OnExecute := @actFindAllExecute;
  fActReplaceNext := TAction.Create(self);
  fActReplaceNext.Caption := 'Replace';
  fActReplaceNext.OnExecute := @actReplaceNextExecute;
  fActReplaceAll := TAction.Create(self);
  fActReplaceAll.Caption := 'Replace all';
  fActReplaceAll.OnExecute := @actReplaceAllExecute;
  //
  fSearchMru := TCEMruList.Create;
  fReplaceMru:= TCEMruList.Create;
  //
  fname := getCoeditDocPath + OptsFname;
  if fname.fileExists then with TCESearchOptions.create(nil) do
  try
    loadFromFile(fname);
    AssignTo(self);
  finally
    free;
  end;
  //
  btnFind.Action := fActFindNext;
  btnReplace.Action := fActReplaceNext;
  btnReplaceAll.Action := fActReplaceAll;
  btnFindAll.Action := fActFindAll;
  AssignPng(btnAllScope, 'document');
  updateImperative;
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCESearchWidget.Destroy;
begin
  with TCESearchOptions.create(nil) do
  try
    Assign(self);
    saveToFile(getCoeditDocPath + OptsFname);
  finally
    free;
  end;
  //
  EntitiesConnector.removeObserver(self);
  fSearchMru.Free;
  fReplaceMru.Free;
  inherited;
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCESearchWidget.getOptions: TSynSearchOptions;
begin
  result := [];
  if chkRegex.Checked     then result += [ssoRegExpr];
  if chkWWord.Checked     then result += [ssoWholeWord];
  if chkBack.Checked      then result += [ssoBackwards];
  if chkCaseSens.Checked  then result += [ssoMatchCase];
  if chkPrompt.Checked    then result += [ssoPrompt];
end;

function dlgReplaceAll: TModalResult;
const
  Btns = [mbYes, mbNo, mbYesToAll, mbNoToAll];
begin
  exit( MessageDlg('Coedit', 'Replace this match ?', mtConfirmation, Btns, ''));
end;

procedure TCESearchWidget.replaceEvent(Sender: TObject; const ASearch, AReplace:
  string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
begin
  case dlgReplaceAll of
    mrYes: ReplaceAction := raReplace;
    mrNo: ReplaceAction := raSkip;
    mrYesToAll: ReplaceAction := raReplaceAll;
    mrCancel, mrClose, mrNoToAll:
      begin
        ReplaceAction := raCancel;
        fCancelAll := true;
      end;
  end;
end;

procedure TCESearchWidget.actFindAllExecute(sender: TObject);
var
  i: integer;
  lst: TSynEditStringList;
  fnm: string;
begin
  if fDoc.isNil and not fAllInProj then
    exit;
  if (fProj = nil) and fAllInProj then
    exit;
  //
  fSearchMru.Insert(0,fToFind);
  cbToFind.Items.Assign(fSearchMru);
  //
  if fAllInProj then
  begin
    lst := TSynEditStringList.Create;
    try
      for i := 0 to fProj.sourcesCount-1 do
      begin
        fnm := fProj.sourceAbsolute(i);
        lst.LoadFromFile(fnm);
        findAll(fnm, lst);
      end;
    finally
      lst.Free;
    end;
  end
  else findAll(fDoc.fileName, fDoc.Lines);
end;

procedure TCESearchWidget.findAll(const filename: string; lines: TStrings);
var
  search: TSynEditSearch;
  options: TSynSearchOptions;
  start, stop: TPoint;
  startf, stopf: TPoint;
  msgs: ICEMessagesDisplay;
  msg: string;
  fmt: string;
  i: integer;
  res: array of TPoint = nil;
begin
  search := TSynEditSearch.Create;
  try
    options := getOptions;
    search.Sensitive := ssoMatchCase in options;
    search.Whole := ssoWholeWord in options;
    search.RegularExpressions:= ssoRegExpr in options;
    search.Pattern:=fToFind;

    // search.IdentChars:= [];

    start := Point(1,1);
    stop := Point(high(integer), lines.Count);
    while search.FindNextOne(lines, start, stop, startf, stopf) do
    begin
      setLength(res, length(res) + 1);
      res[high(res)].X := startf.X;
      res[high(res)].Y := startf.Y;
      start := stopf;
    end;
    msgs := getMessageDisplay;
    msg := format('%d result(s) for the pattern <%s>', [length(res), fToFind]);
    msgs.message(msg, nil, amcMisc, amkInf);
    fmt := fileName + '(%d,%d): "%s"';
    for i := 0 to high(res) do
    begin
      msg := format(fmt, [res[i].Y, res[i].X, Trim(lines[res[i].Y-1])]);
      msgs.message(msg, nil, amcMisc, amkInf);
    end;
  finally
    search.free;
  end;
end;

procedure TCESearchWidget.actFindNextExecute(sender: TObject);
begin
  if fDoc.isNil then exit;
  //
  fSearchMru.Insert(0,fToFind);
  cbToFind.Items.Assign(fSearchMru);
  //
  if not chkFromCur.Checked then
  begin
    if chkBack.Checked then
      fDoc.CaretXY := Point(high(Integer), high(Integer))
    else
    begin
      if not fHasRestarted then
        fDoc.CaretXY := Point(0,0);
      fHasRestarted := true;
    end;
  end
  else if fHasSearched then
  begin
    if chkBack.Checked then
      fDoc.CaretX := fDoc.CaretX - 1
    else
      fDoc.CaretX := fDoc.CaretX + fToFind.length;
  end;
  if fDoc.SearchReplace(fToFind, '', getOptions) = 0 then
    dlgOkInfo('the expression cannot be found')
  else
  begin
    fHasSearched := true;
    fHasRestarted := false;
    chkFromCur.Checked := true;
  end;
  updateImperative;
end;

procedure TCESearchWidget.actReplaceNextExecute(sender: TObject);
begin
  if fDoc.isNil then exit;
  //
  fSearchMru.Insert(0, fToFind);
  fReplaceMru.Insert(0, fReplaceWth);
  cbToFind.Items.Assign(fSearchMru);
  cbReplaceWth.Items.Assign(fReplaceMru);
  //
  if chkPrompt.Checked then
    fDoc.OnReplaceText := @replaceEvent;
  if not chkFromCur.Checked then
  begin
    if chkBack.Checked then
      fDoc.CaretXY := Point(high(Integer), high(Integer))
    else
      fDoc.CaretXY := Point(0,0);
  end
  else if fHasSearched then
  begin
    if chkBack.Checked then
      fDoc.CaretX := fDoc.CaretX - 1
    else
      fDoc.CaretX := fDoc.CaretX + fToFind.length;
  end;
  if fDoc.SearchReplace(fToFind, fReplaceWth, getOptions + [ssoReplace]) <> 0 then
    fHasSearched := true;
  fDoc.OnReplaceText := nil;
  updateImperative;
end;

procedure TCESearchWidget.actReplaceAllExecute(sender: TObject);
var
  opts: TSynSearchOptions;
begin
  if fDoc.isNil then exit;
  cbReplaceWth.Items.Assign(fReplaceMru);
  opts := getOptions + [ssoReplace];
  opts -= [ssoBackwards];
  //
  fSearchMru.Insert(0, fToFind);
  fReplaceMru.Insert(0, fReplaceWth);
  if chkPrompt.Checked then fDoc.OnReplaceText := @replaceEvent;
  fDoc.CaretXY := Point(0,0);
  while(true) do
  begin
    if fDoc.SearchReplace(fToFind, fReplaceWth, opts) = 0
      then break;
    if fCancelAll then
    begin
      fCancelAll := false;
      break;
    end;
  end;
  fDoc.OnReplaceText := nil;
  updateImperative;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCESearchWidget.projNew(aProject: ICECommonProject);
begin
  fProj := aProject;
  updateImperative;
end;

procedure TCESearchWidget.projChanged(aProject: ICECommonProject);
begin
end;

procedure TCESearchWidget.projClosing(aProject: ICECommonProject);
begin
  if fProj = aProject then
    fProj := nil;
  updateImperative;
end;

procedure TCESearchWidget.projFocused(aProject: ICECommonProject);
begin
  fProj := aProject;
  updateImperative;
end;

procedure TCESearchWidget.projCompiling(aProject: ICECommonProject);
begin
end;

procedure TCESearchWidget.projCompiled(aProject: ICECommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCESearchWidget.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  updateImperative;
end;

procedure TCESearchWidget.docClosing(aDoc: TCESynMemo);
begin
  if fDoc = aDoc then fDoc := nil;
  updateImperative;
end;

procedure TCESearchWidget.docFocused(aDoc: TCESynMemo);
begin
  if fDoc = aDoc then exit;
  fDoc := aDoc;
  updateImperative;
end;

procedure TCESearchWidget.docChanged(aDoc: TCESynMemo);
begin
end;
{$ENDREGION}

{$REGION Misc. -----------------------------------------------------------------}
procedure TCESearchWidget.cbToFindChange(Sender: TObject);
begin
  if Updating then exit;
  fToFind := cbToFind.Text;
  fHasSearched := false;
  updateImperative;
end;

procedure TCESearchWidget.chkEnableRepChange(Sender: TObject);
begin
  if Updating then exit;
  updateImperative;
end;

procedure TCESearchWidget.cbReplaceWthChange(Sender: TObject);
begin
  if Updating then exit;
  fReplaceWth := cbReplaceWth.Text;
  fHasSearched := false;
  updateImperative;
end;

procedure TCESearchWidget.btnAllScopeClick(Sender: TObject);
begin
  fAllInProj := not fAllInProj;
  if fAllInProj then
  begin
    AssignPng(btnAllScope, 'document_all');
    btnAllScope.Hint := 'all project sources';
  end
  else
  begin
    AssignPng(btnAllScope, 'document');
    btnAllScope.Hint := 'selected source';
  end;
  updateImperative;
end;

procedure TCESearchWidget.updateImperative;
var
  canAll: boolean;
begin
  canAll := (fDoc.isNotNil and not fAllInProj) or (fAllInProj and (fProj <> nil));
  btnFind.Enabled := fDoc.isNotNil and fToFind.isNotEmpty;
  btnFindAll.Enabled := canAll;
  btnReplace.Enabled := fDoc.isNotNil and chkEnableRep.Checked and fToFind.isNotEmpty;
  btnReplaceAll.Enabled := btnReplace.Enabled;
  cbReplaceWth.Enabled := fDoc.isNotNil and chkEnableRep.Checked;
  cbToFind.Enabled := canAll;
end;
{$ENDREGION}

initialization
  RegisterClasses([TCESearchOptions]);
end.
