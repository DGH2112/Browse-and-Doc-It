unit BADI.VBEIDE.ActiveForm;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, BrowseAndDocItVBEIDE64_TLB, StdVcl, Vcl.ExtCtrls;

type
  TTBADIActiveXToolWndForm = class(TActiveForm, ITBADIActiveXToolWndForm)
    pnl: TPanel;
    procedure ActiveFormDestroy(Sender: TObject);
    procedure ActiveFormCreate(Sender: TObject);
  private
    { Private declarations }
    FEvents: ITBADIActiveXToolWndFormEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure AfterMonitorDpiChangedEvent(Sender: TObject; OldDPI, NewDPI: Integer);
    procedure BeforeMonitorDpiChangedEvent(Sender: TObject; OldDPI, NewDPI: Integer);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure MouseEnterEvent(Sender: TObject);
    procedure MouseLeaveEvent(Sender: TObject);
    procedure PaintEvent(Sender: TObject);
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AlignWithMargins: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_BorderWidth: Integer; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_CurrentPPI: Integer; safecall;
    function Get_DockSite: WordBool; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_ExplicitHeight: Integer; safecall;
    function Get_ExplicitLeft: Integer; safecall;
    function Get_ExplicitTop: Integer; safecall;
    function Get_ExplicitWidth: Integer; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_IsDrawingLocked: WordBool; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_MouseInClient: WordBool; safecall;
    function Get_ParentCustomHint: WordBool; safecall;
    function Get_ParentDoubleBuffered: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PopupMode: TxPopupMode; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_RaiseOnNonMainThreadUsage: WordBool; safecall;
    function Get_RedrawDisabled: WordBool; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScaleFactor: Single; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_StyleName: WideString; safecall;
    function Get_UseDockManager: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    procedure Set_AlignWithMargins(Value: WordBool); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_BorderWidth(Value: Integer); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_DockSite(Value: WordBool); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_ParentCustomHint(Value: WordBool); safecall;
    procedure Set_ParentDoubleBuffered(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PopupMode(Value: TxPopupMode); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_RaiseOnNonMainThreadUsage(Value: WordBool); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    procedure Set_StyleName(const Value: WideString); safecall;
    procedure Set_UseDockManager(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
  public
    { Public declarations }
    procedure Initialize; override;
  end;

implementation

uses
  System.Win.ComObj,
  ComServ,
  CodeSiteLogging;

{$R *.DFM}

procedure TTBADIActiveXToolWndForm.ActiveFormDestroy(Sender: TObject);
begin
  CodeSite.TraceMethod(Self, 'ActiveFormDestroy', tmoTiming);
end;

procedure TTBADIActiveXToolWndForm.ActiveFormCreate(Sender: TObject);
begin
  CodeSite.TraceMethod(Self, 'ActiveFormCreate', tmoTiming);
end;

{ TTBADIActiveXToolWndForm }

procedure TTBADIActiveXToolWndForm.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_TBADIActiveXToolWndFormPage); }
end;

procedure TTBADIActiveXToolWndForm.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ITBADIActiveXToolWndFormEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TTBADIActiveXToolWndForm.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnAfterMonitorDpiChanged := AfterMonitorDpiChangedEvent;
  OnBeforeMonitorDpiChanged := BeforeMonitorDpiChangedEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnMouseEnter := MouseEnterEvent;
  OnMouseLeave := MouseLeaveEvent;
  OnPaint := PaintEvent;
end;

function TTBADIActiveXToolWndForm.Get_Active: WordBool;
begin
  Result := Active;
end;

function TTBADIActiveXToolWndForm.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TTBADIActiveXToolWndForm.Get_AlignWithMargins: WordBool;
begin
  Result := AlignWithMargins;
end;

function TTBADIActiveXToolWndForm.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TTBADIActiveXToolWndForm.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TTBADIActiveXToolWndForm.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TTBADIActiveXToolWndForm.Get_BorderWidth: Integer;
begin
  Result := Integer(BorderWidth);
end;

function TTBADIActiveXToolWndForm.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TTBADIActiveXToolWndForm.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TTBADIActiveXToolWndForm.Get_CurrentPPI: Integer;
begin
  Result := CurrentPPI;
end;

function TTBADIActiveXToolWndForm.Get_DockSite: WordBool;
begin
  Result := DockSite;
end;

function TTBADIActiveXToolWndForm.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TTBADIActiveXToolWndForm.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TTBADIActiveXToolWndForm.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TTBADIActiveXToolWndForm.Get_ExplicitHeight: Integer;
begin
  Result := ExplicitHeight;
end;

function TTBADIActiveXToolWndForm.Get_ExplicitLeft: Integer;
begin
  Result := ExplicitLeft;
end;

function TTBADIActiveXToolWndForm.Get_ExplicitTop: Integer;
begin
  Result := ExplicitTop;
end;

function TTBADIActiveXToolWndForm.Get_ExplicitWidth: Integer;
begin
  Result := ExplicitWidth;
end;

function TTBADIActiveXToolWndForm.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TTBADIActiveXToolWndForm.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TTBADIActiveXToolWndForm.Get_IsDrawingLocked: WordBool;
begin
  Result := IsDrawingLocked;
end;

function TTBADIActiveXToolWndForm.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TTBADIActiveXToolWndForm.Get_MouseInClient: WordBool;
begin
  Result := MouseInClient;
end;

function TTBADIActiveXToolWndForm.Get_ParentCustomHint: WordBool;
begin
  Result := ParentCustomHint;
end;

function TTBADIActiveXToolWndForm.Get_ParentDoubleBuffered: WordBool;
begin
  Result := ParentDoubleBuffered;
end;

function TTBADIActiveXToolWndForm.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TTBADIActiveXToolWndForm.Get_PopupMode: TxPopupMode;
begin
  Result := Ord(PopupMode);
end;

function TTBADIActiveXToolWndForm.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TTBADIActiveXToolWndForm.Get_RaiseOnNonMainThreadUsage: WordBool;
begin
  Result := RaiseOnNonMainThreadUsage;
end;

function TTBADIActiveXToolWndForm.Get_RedrawDisabled: WordBool;
begin
  Result := RedrawDisabled;
end;

function TTBADIActiveXToolWndForm.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TTBADIActiveXToolWndForm.Get_ScaleFactor: Single;
begin
  Result := ScaleFactor;
end;

function TTBADIActiveXToolWndForm.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TTBADIActiveXToolWndForm.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TTBADIActiveXToolWndForm.Get_StyleName: WideString;
begin
  Result := WideString(StyleName);
end;

function TTBADIActiveXToolWndForm.Get_UseDockManager: WordBool;
begin
  Result := UseDockManager;
end;

function TTBADIActiveXToolWndForm.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TTBADIActiveXToolWndForm.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TTBADIActiveXToolWndForm._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TTBADIActiveXToolWndForm.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TTBADIActiveXToolWndForm.AfterMonitorDpiChangedEvent(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  if FEvents <> nil then FEvents.OnAfterMonitorDpiChanged(OldDPI, NewDPI);
end;

procedure TTBADIActiveXToolWndForm.BeforeMonitorDpiChangedEvent(Sender: TObject; OldDPI, NewDPI: Integer);
          
begin
  if FEvents <> nil then FEvents.OnBeforeMonitorDpiChanged(OldDPI, NewDPI);
end;

procedure TTBADIActiveXToolWndForm.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TTBADIActiveXToolWndForm.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TTBADIActiveXToolWndForm.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TTBADIActiveXToolWndForm.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TTBADIActiveXToolWndForm.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TTBADIActiveXToolWndForm.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TTBADIActiveXToolWndForm.MouseEnterEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnMouseEnter;
end;

procedure TTBADIActiveXToolWndForm.MouseLeaveEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnMouseLeave;
end;

procedure TTBADIActiveXToolWndForm.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TTBADIActiveXToolWndForm.Set_AlignWithMargins(Value: WordBool);
begin
  AlignWithMargins := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_BorderWidth(Value: Integer);
begin
  BorderWidth := TBorderWidth(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_DockSite(Value: WordBool);
begin
  DockSite := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TTBADIActiveXToolWndForm.Set_HelpFile(const Value: WideString);
begin
  HelpFile := string(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_ParentCustomHint(Value: WordBool);
begin
  ParentCustomHint := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_ParentDoubleBuffered(Value: WordBool);
begin
  ParentDoubleBuffered := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_PopupMode(Value: TxPopupMode);
begin
  PopupMode := TPopupMode(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_RaiseOnNonMainThreadUsage(Value: WordBool);
begin
  RaiseOnNonMainThreadUsage := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_StyleName(const Value: WideString);
begin
  StyleName := string(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_UseDockManager(Value: WordBool);
begin
  UseDockManager := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TTBADIActiveXToolWndForm,
    Class_TBADIActiveXToolWndForm,
    0,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.

