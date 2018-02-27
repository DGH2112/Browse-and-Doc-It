unit uAddin;

interface

uses
  Classes, Windows, ComObj, ActiveX, BFOutlook_TLB, StdVcl, Registry,
  contnrs, Sysutils, ShellAPI, Graphics, clipbrd, Forms, Dialogs, Buttons,
  Controls, extctrls, Variants,
  AddInDesignerObjects_TLB, uOutlookEvents, Outlook_TLB, Office_TLB,
  uSaveClipboard, fTranslate, dlgWait;

type

  TAddinInspector = class(TOutlookInspector)
  private
    FButtons: TObjectList;
  public
    constructor Create(svrIntf: IUnknown); override;
    destructor Destroy; override;
    property Buttons : TObjectList read FButtons;
  end;

  TAddinExplorer = class(TOutlookExplorer)
  private
    FButtons: TObjectList;
  public
    constructor Create(svrIntf: IUnknown); override;
    destructor Destroy; override;
    property Buttons : TObjectList read FButtons;
  end;

  TAddinOutlookButton = class(TOutlookCommandBarButton)
  private
    FInspector: TAddinInspector;
    FData: string;
    FExplorer: TAddinExplorer;
  public
    property Inspector : TAddinInspector  read FInspector write FInspector;
    property Explorer : TAddinExplorer read FExplorer write FExplorer;
    property Data : string read FData write FData;
  end;

  TBabelFishFactory = class(TAutoObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  TBabelFish = class(TAutoObject, IBabelFish, _IDTExtensibility2)
  private
    FApplication : OleVariant;
    FInspectors : TOutlookInspectors;
    FInspectorsList : TObjectList;
    FExplorers : TOutlookExplorers;
    FExplorersList : TObjectList;
    FOutlookApplication : TOutlookApplication;
  protected
    { Protected declarations }
  public
    //_IDTExtensibility2
    procedure OnConnection(const Application: IDispatch; ConnectMode: ext_ConnectMode;
                           const AddInInst: IDispatch; var custom: PSafeArray); safecall;
    procedure OnDisconnection(RemoveMode: ext_DisconnectMode; var custom: PSafeArray); safecall;
    procedure OnAddInsUpdate(var custom: PSafeArray); safecall;
    procedure OnStartupComplete(var custom: PSafeArray); safecall;
    procedure OnBeginShutdown(var custom: PSafeArray); safecall;
    //VCL
    procedure AfterConstruction;override;
    destructor Destroy;override;
    procedure InstallExplorerToolbar(Explorer : TAddinExplorer);
    procedure OnNewInspector(Sender : TOutlookInspectors; const Inspector : _Inspector);
    procedure OnNewExplorer(Sender : TOutlookExplorers;const Explorer: _Explorer);
    procedure OnInspectorClose(Sender : TOutlookInspector);
    procedure OnExplorerClose(Sender : TObject);
    procedure OnExplorerActivate(Sender : TObject);
    procedure OnSelectionChange(Sender : TObject);
    procedure OnOptionsPagesAdd(Sender : TOutlookApplication; const Pages: PropertyPages);
    procedure OnTranslateClick(Sender : TOutlookCommandBarButton; const Ctrl: CommandBarButton; var CancelDefault: WordBool);
    procedure OnBabelFishOnTheWebClick(Sender : TOutlookCommandBarButton; const Ctrl: CommandBarButton; var CancelDefault: WordBool);
  end;

implementation

uses ComServ;

{ TBabelFishFactory }

procedure TBabelFishFactory.UpdateRegistry(Register: Boolean);
var Reg:TRegistry;
begin
  inherited;
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;//HKEY_CURRENT_USER;
    if Register then begin
      if Reg.OpenKey('Software\Microsoft\Office\Outlook\Addins\'+GetProgID, TRUE) then begin
        Reg.WriteString('FriendlyName', 'BabelFish for Outlook');
        Reg.WriteInteger('LoadBehavior', 3);
      end;
    end
    else begin
      if Reg.KeyExists('Software\Microsoft\Office\Outlook\Addins\'+GetProgID) then begin
        Reg.DeleteKey('Software\Microsoft\Office\Outlook\Addins\'+GetProgID);
      end;
    end;
  finally
    Reg.Free;
  end;
end;

{ TBabelFish }

procedure TBabelFish.AfterConstruction;
begin
  inherited;
  FInspectorsList := TObjectList.Create(TRUE);
  FExplorersList := TObjectList.Create(TRUE);
  FInspectors:=nil;
  FExplorers:=nil;
  FOutlookApplication:=nil;
end;

destructor TBabelFish.Destroy;
begin
  FreeAndNil(FInspectors);
  FreeAndNil(FInspectorsList);
  FreeAndNil(FExplorers);
  FreeAndNil(FExplorersList);
  FreeAndNil(FOutlookApplication);
  inherited;
end;

procedure TBabelFish.OnAddInsUpdate(var custom: PSafeArray);
begin
  //nothing
end;

procedure TBabelFish.OnBabelFishOnTheWebClick(
  Sender: TOutlookCommandBarButton; const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);
begin
  ShellExecute(GetForegroundWindow, 'open', 'http://babelfish.altavista.com', nil, nil, SW_SHOW);
end;

procedure TBabelFish.OnBeginShutdown(var custom: PSafeArray);
begin
  //nothing
end;

procedure TBabelFish.OnConnection(const Application: IDispatch;
  ConnectMode: ext_ConnectMode; const AddInInst: IDispatch;
  var custom: PSafeArray);
var i : integer;
begin
  FApplication:=Application;
  FOutlookApplication:=TOutlookApplication.Create(Application);
  FOutlookApplication.OnOptionsPagesAdd:=OnOptionsPagesAdd;
  FInspectors:=TOutlookInspectors.Create(FApplication.Inspectors);
  FInspectors.OnNewInspector:=OnNewInspector;
  for i:=1 to FApplication.Inspectors.Count do begin
    OnNewInspector(FInspectors, IDispatch(FApplication.Inspectors.Item(i)) as _Inspector);
  end;
  FExplorers:=TOutlookExplorers.Create(FApplication.Explorers);
  FExplorers.OnNewExplorer:=OnNewExplorer;
  for i:=1 to FApplication.Explorers.Count do begin
    OnNewExplorer(FExplorers, IDispatch(FApplication.Explorers.Item(i)) as _Explorer);
  end;
end;

procedure TBabelFish.OnDisconnection(RemoveMode: ext_DisconnectMode;
  var custom: PSafeArray);
begin
  FreeAndNil(FInspectors);
  FreeAndNil(FExplorers);
  FreeAndNil(FOutlookApplication);
  FApplication:=Unassigned;
end;

procedure TBabelFish.OnExplorerClose(Sender: TObject);
var ind : integer;
begin
  ind:=FExplorersList.IndexOf(Sender);
  if ind >= 0 then FExplorersList.Delete(ind);
end;

procedure TBabelFish.OnInspectorClose(Sender: TOutlookInspector);
var ind : integer;
begin
  ind:=FInspectorsList.IndexOf(Sender);
  if ind >= 0 then FInspectorsList.Delete(ind);
end;

procedure BmpToBtn(ResName : string; Button : OleVariant);
var bmp:TBitmap;
    SaveClipboard:TSaveClipboard;
    icon:TIcon;
    ResStream:TResourceStream;
    BtnStyle:integer;
    //strBtnStyle:string;
begin
  try
    ResStream:=TResourceStream.Create(HInstance, ResName, RT_RCDATA);
    ResStream.Position:=0;
    icon:=TIcon.Create;
    SaveClipboard:=TSaveClipboard.Create;
    try
      SaveClipboard.Save;
      icon.LoadFromStream(ResStream);
      bmp:=TBitmap.Create;
      SaveClipboard.Save;
      try
        bmp.width:=16;
        bmp.height:=16;
        bmp.Canvas.Brush.Color:=clBtnFace;
        bmp.Canvas.FillRect(Rect(0, 0, bmp.width, bmp.height));
        bmp.Canvas.Draw(0, 0, icon);
        Clipboard.Assign(bmp);
        BtnStyle:=3; //msoButtonIconAndCaption
        //strBtnStyle:=gOptions.Values['Button_'+Button.Caption+'_Style'];
        Button.Style:=BtnStyle;
        Button.PasteFace;
      finally
        bmp.Free;
      end;
    finally
      icon.Free;
      SaveClipboard.Restore;
      SaveClipboard.Free;
    end;
  except
  end;
end;

procedure TBabelFish.InstallExplorerToolbar(Explorer: TAddinExplorer);
function AddItem(Explorer : TAddinExplorer; Toolbar : OleVariant;
                   Caption : string; BmpName : string; Data : string;
                   Type_ : integer; BeginGroup : boolean) : TAddinOutlookButton;
  var Btn : OleVariant;
  begin
    try
      Btn:=Toolbar.Controls.Item[Caption];
    except
      Btn:=Toolbar.Controls.Add(Type:=Type_, Temporary:=true);
    end;
    Btn.Caption:=Caption;
    Btn.BeginGroup:=BeginGroup;
    Btn.Tag:=Caption+'_'+IntToStr(Random(MaxInt));
    if Length(BmpName) > 0 then BmpToBtn(BmpName, Btn);
    Result:=TAddinOutlookButton.Create(IUnknown(Btn));
    Explorer.Buttons.Add(Result);
    Result.Explorer:=Explorer;
    Result.Data:=Data;
    Result.OnClick:=OnTranslateClick;
  end;

var Popup : OleVariant;
    Toolbar : OleVariant;
begin
  //add "Translate" to the "Edit" menu
  try
    //we can fail on the next line if the inspector is not yet visible and has no toolbars yet,
    //but we will check our button in OnActivate() anyawy
    Toolbar:=OleVariant(Explorer.Intf as IDispatch).CommandBars.Item['Standard'];
    if (VarType(Toolbar) = varDispatch) and (IDispatch(Toolbar) <> nil) then begin
      //Popup:=AddItem(Insp, Toolbar, 'Translate', '', '', 10, true).Intf as IDispatch;
      with AddItem(Explorer, Toolbar, 'Translate', '', '', 10, true) do begin
        Popup:=Intf as IDispatch;
      end;
      AddItem(Explorer, Popup, 'English -> French', '', 'en_fr', 1, true);
      AddItem(Explorer, Popup, 'English -> German', '', 'en_de', 1, false);
      AddItem(Explorer, Popup, 'English -> Italian', '', 'en_it', 1, false);
      AddItem(Explorer, Popup, 'English -> Spanish', '', 'en_es', 1, false);
      AddItem(Explorer, Popup, 'English -> Portugese', '', 'en_pt', 1, false);
      AddItem(Explorer, Popup, 'French -> English', '', 'fr_en', 1, true);
      AddItem(Explorer, Popup, 'German -> English', '', 'de_en', 1, false);
      AddItem(Explorer, Popup, 'Italian -> English', '', 'it_en', 1, false);
      AddItem(Explorer, Popup, 'Portugese -> English', '', 'pt_en', 1, false);
      AddItem(Explorer, Popup, 'Russian -> English', '', 'ru_en', 1, false);
      AddItem(Explorer, Popup, 'Spanish -> English', '', 'es_en', 1, false);
      with AddItem(Explorer, Popup, 'BabelFish on the Web', 'fish', '', 1, true) do begin
        OnClick:=OnBabelFishOnTheWebClick;
      end;
    end;
  except
  end;
end;

procedure TBabelFish.OnNewExplorer(Sender: TOutlookExplorers;
  const Explorer: _Explorer);
var Expl : TAddinExplorer;
begin
  Expl := TAddinExplorer.Create(Explorer);
  Expl.OnClose:=OnExplorerClose;
  Expl.OnSelectionChange:=OnSelectionChange;
  Expl.OnActivate:=OnExplorerActivate;
  FExplorersList.Add(Expl);
  InstallExplorerToolbar(Expl); //this can fail is the explorer is not uet visible
end;


procedure TBabelFish.OnNewInspector(Sender: TOutlookInspectors; const Inspector: _Inspector);

  function AddItem(Inspector : TAddinInspector; Toolbar : OleVariant;
                   Caption : string; BmpName : string; Data : string;
                   Type_ : integer; BeginGroup : boolean) : TAddinOutlookButton;
  var Btn : OleVariant;
  begin
    try
      Btn:=Toolbar.Controls.Item[Caption];
    except
      Btn:=Toolbar.Controls.Add(Type:=Type_, Temporary:=true);
    end;
    Btn.Caption:=Caption;
    Btn.BeginGroup:=BeginGroup;
    Btn.Tag:=Caption+'_'+IntToStr(Random(MaxInt));
    if Length(BmpName) > 0 then BmpToBtn(BmpName, Btn);
    Result:=TAddinOutlookButton.Create(IUnknown(Btn));
    Inspector.Buttons.Add(Result);
    Result.Inspector:=Inspector;
    Result.Data:=Data;
    Result.OnClick:=OnTranslateClick;
  end;

var Insp : TAddinInspector;
    Popup : OleVariant;
    Toolbar : OleVariant;
begin
  Insp := TAddinInspector.Create(Inspector);
  Insp.OnClose:=OnInspectorClose;
  FInspectorsList.Add(Insp);
  //add "Translate" to the "Edit" menu
  try
    Toolbar:=Inspector.CommandBars.Item['Standard'];
    if (VarType(Toolbar) = varDispatch) and (IDispatch(Toolbar) <> nil) then begin
      //Popup:=AddItem(Insp, Toolbar, 'Translate', '', '', 10, true).Intf as IDispatch;
      with AddItem(Insp, Toolbar, 'Translate', '', '', 10, true) do begin
        Popup:=Intf as IDispatch;
      end;
      AddItem(Insp, Popup, 'English -> French', '', 'en_fr', 1, true);
      AddItem(Insp, Popup, 'English -> German', '', 'en_de', 1, false);
      AddItem(Insp, Popup, 'English -> Italian', '', 'en_it', 1, false);
      AddItem(Insp, Popup, 'English -> Spanish', '', 'en_es', 1, false);
      AddItem(Insp, Popup, 'English -> Portugese', '', 'en_pt', 1, false);
      AddItem(Insp, Popup, 'French -> English', '', 'fr_en', 1, true);
      AddItem(Insp, Popup, 'German -> English', '', 'de_en', 1, false);
      AddItem(Insp, Popup, 'Italian -> English', '', 'it_en', 1, false);
      AddItem(Insp, Popup, 'Portugese -> English', '', 'pt_en', 1, false);
      AddItem(Insp, Popup, 'Russian -> English', '', 'ru_en', 1, false);
      AddItem(Insp, Popup, 'Spanish -> English', '', 'es_en', 1, false);
      with AddItem(Insp, Popup, 'BabelFish on the Web', 'fish', '', 1, true) do begin
        OnClick:=OnBabelFishOnTheWebClick;
      end;
    end;
  except
  end;
end;

procedure TBabelFish.OnSelectionChange(Sender: TObject);
var varExpl, varBtn : OleVariant;
    bEnable : boolean;
    i : integer;
begin
  //disable "Translate" button on this explorer if nothing is selected
  try
    if (Sender is TOutlookExplorer) then begin
      varExpl:=TAddinExplorer(Sender).Intf as IDispatch;
      bEnable:=(varExpl.Selection.Count > 0);
      //find the "Translate" button
      for i:=0 to TAddinExplorer(Sender).Buttons.Count-1 do begin
        varBtn:=TOutlookCommandBarButton(TAddinExplorer(Sender).Buttons[i]).Intf as IDispatch;
        if CompareText('Translate', varBtn.Caption) = 0 then begin
          varBtn.Enabled:=bEnable;
          Break;
        end;
      end;
    end;
  except
    //oops...
  end;
end;

procedure TBabelFish.OnStartupComplete(var custom: PSafeArray);
begin
  //nothing
end;

function HasText(const Value : string):boolean;
var i : integer;
begin
  Result:=false;
  for i:=1 to Length(Value) do
    if not (Value[i] in [#$8, #$20, #160{?}, #13, #10, '.', ',', ':', '-', '0'..'9']) then begin
      Result:=true;
      Break;
    end;
end;

function StripCRLF(const Value : string):string;
begin
  Result:=StringReplace(StringReplace(Value, #13, '', [rfReplaceAll]), #10, '', [rfReplaceAll]);
end;

procedure TBabelFish.OnTranslateClick(Sender: TOutlookCommandBarButton;
  const Ctrl: CommandBarButton; var CancelDefault: WordBool);
var wnd : HWND;
    OW : IOleWindow;
    varHTMLEditor, varWordEditor, varExplorer : OleVariant;
    strText, strOutput : string;
begin
  try
    strText:='';
    //is this button from an inspector or an explorer?
    if TAddinOutlookButton(Sender).Inspector <> nil then begin
      //INSPECTOR
      //get inspector HWND
      if (S_OK = TAddinOutlookButton(Sender).Inspector.Intf.QueryInterface(IOleWindow, OW)) and
         (S_OK = OW.GetWindow(wnd))
      then begin
        //we've got our wnd
      end
      else begin
        wnd:=GetForegroundWindow;
      end;
      //get the selected text
      try
        varHTMLEditor:=OleVariant(TAddinOutlookButton(Sender).Inspector.Intf as IDispatch).HTMLEditor;
      except
        varHTMLEditor:=Unassigned;
      end;
      if (VarType(varHTMLEditor) = varDispatch) and (IDispatch(varHTMLEditor) <> nil) then begin
        //HTML editor
        strText:=varHTMLEditor.Selection.createRange.text;
        if not HasText(strText){Length(StripCRLF(strText)) = 0} then strText:=varHTMLEditor.body.createTextRange.text;
      end
      else begin
        try
          varWordEditor:=OleVariant(TAddinOutlookButton(Sender).Inspector.Intf as IDispatch).WordEditor;
        except
          varWordEditor:=Unassigned;
        end;
        if (VarType(varWordEditor) = varDispatch) and (IDispatch(varWordEditor) <> nil) then begin
          //Word editor
          strText:=varWordEditor.Application.Selection.Text;
          if not HasText(strText){Length(StripCRLF(strText)) = 0} then strText:=varWordEditor.Content.Text;
        end
        else begin
          //we have an Outlook editor, hack it...
          //no selection...
          //todo: try to get the selection anyway
          strText:=OleVariant(TAddinOutlookButton(Sender).Inspector.Intf as IDispatch).CurrentItem.Body;
        end;
      end;
    end
    else if TAddinOutlookButton(Sender).Explorer <> nil then begin
      //EXPLORER
      //get explorer HWND
      if (S_OK = TAddinOutlookButton(Sender).Explorer.Intf.QueryInterface(IOleWindow, OW)) and
         (S_OK = OW.GetWindow(wnd))
      then begin
        //we've got our wnd
      end
      else begin
        wnd:=GetForegroundWindow;
      end;
      //get the body of the first selected e-mail
      varExplorer:=TAddinOutlookButton(Sender).Explorer.Intf as IDispatch;
      if varExplorer.Selection.Count = 0 then raise SysUtils.Exception.Create('No e-mail is selected');
      strText:=varExplorer.Selection.Item(1).Body;
    end;
    if not HasText(strText) then raise SysUtils.Exception.Create('Nothing to translate...');
    //translate
    with TWaitDlg.Create(nil, wnd) do
      try
        //can raise exceptions...
        strOutput:=Translate(strText, TAddinOutlookButton(Sender).Data);
      finally
        Release;
      end;
    //todo:
    //show the results
    with TdlgTranslate.Create(nil, wnd) do begin
      OriginalTxt.Text:=strText;
      TranslationTxt.Text:=strOutput;
      Caption:='Translate '+Ctrl.Caption;
      if ShowModal = mrOk then begin
        //todo: replace?
      end;
      Free;
    end;
  except
    on E:SysUtils.Exception do MessageBox(wnd, PChar(E.Message), 'Error', MB_OK or MB_ICONERROR);
  end;
end;

procedure TBabelFish.OnExplorerActivate(Sender: TObject);
begin
  try
    //check if the explorer already has our buttons.
    //if not, add them
    if (Sender is TAddinExplorer) then begin
      if TAddinExplorer(Sender).Buttons.Count = 0 then begin
        InstallExplorerToolbar(TAddinExplorer(Sender));
      end;
    end;
  except
    //oops...
  end;
end;

procedure TBabelFish.OnOptionsPagesAdd(Sender: TOutlookApplication;
  const Pages: PropertyPages);
begin
  try
    Pages.Add('BFOutlook.ToolsOptionsPage', 'BabelFish');
  except
    //oops...
  end;
end;

{ TAddinInspector }

constructor TAddinInspector.Create(svrIntf: IUnknown);
begin
  inherited;
  FButtons:=TObjectList.Create(TRUE);
end;

destructor TAddinInspector.Destroy;
begin
  FButtons.Free;
  inherited;
end;

{ TAddinExplorer }

constructor TAddinExplorer.Create(svrIntf: IUnknown);
begin
  inherited;
  FButtons:=TObjectList.Create(TRUE);
end;

destructor TAddinExplorer.Destroy;
begin
  FButtons.Free;
  inherited;
end;

initialization
  TBabelFishFactory.Create(ComServer, TBabelFish, Class_BabelFish,
    ciMultiInstance, tmApartment);
end.
