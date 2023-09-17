unit BADI.VBEIDE.ActiveForm;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Win.StdVCL,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.AxCtrls,
  Vcl.ExtCtrls,
  {$IFDEF WIN32}
  BrowseAndDocItVBEIDE_TLB;
  {$ELSE}
  BrowseAndDocItVBEIDE64_TLB;
  {$ENDIF}

type
  TTBADIActiveXToolWndForm = class(TActiveForm, ITBADIActiveXToolWndForm)
    tmUpdateBounds: TTimer;
    procedure ActiveFormCreate(Sender: TObject);
    procedure tmUpdateBoundsTimer(Sender: TObject);
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
    function Get_VCLFormRef: TBADIActiveXToolWndForm; safecall;
    procedure Set_VCLFormRef(const Value: TBADIActiveXToolWndForm); safecall;
    Procedure WMResize(var M : TWMSize); Message WM_SIZE;
    Procedure WMResizing(var M : TMessage); Message WM_SIZING;
    Procedure WMWindowPosChanged(var M : TMessage); Message WM_WINDOWPOSCHANGING;
    Procedure WMMove(var M : TWMMove); Message WM_MOVE;
    Procedure WMMoving(var M : TWMMoving); Message WM_MOVING;
  public
    { Public declarations }
    procedure Initialize; override;
    Procedure UpdateBounds();
  end;

implementation

uses
  System.Win.ComObj,
  ComServ,
  CodeSiteLogging;

{$R *.DFM}

procedure TTBADIActiveXToolWndForm.ActiveFormCreate(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ActiveFormCreate', tmoTiming);{$ENDIF}
end;

{ TTBADIActiveXToolWndForm }

procedure TTBADIActiveXToolWndForm.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'DefinePropertyPages', tmoTiming);{$ENDIF}
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_TBADIActiveXToolWndFormPage); }
end;

procedure TTBADIActiveXToolWndForm.EventSinkChanged(const EventSink: IUnknown);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EventSinkChanged', tmoTiming);{$ENDIF}
  FEvents := EventSink as ITBADIActiveXToolWndFormEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TTBADIActiveXToolWndForm.Initialize;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Initialize', tmoTiming);{$ENDIF}
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
  DoubleBuffered := True;
  tmUpdateBounds.Enabled := True;
end;

function TTBADIActiveXToolWndForm.Get_Active: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_Active', tmoTiming);{$ENDIF}
  Result := Active;
end;

function TTBADIActiveXToolWndForm.Get_AlignDisabled: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_AlignDisabled', tmoTiming);{$ENDIF}
  Result := AlignDisabled;
end;

function TTBADIActiveXToolWndForm.Get_AlignWithMargins: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_AlignWithMargins', tmoTiming);{$ENDIF}
  Result := AlignWithMargins;
end;

function TTBADIActiveXToolWndForm.Get_AutoScroll: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_AutoScroll', tmoTiming);{$ENDIF}
  Result := AutoScroll;
end;

function TTBADIActiveXToolWndForm.Get_AutoSize: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_AutoSize', tmoTiming);{$ENDIF}
  Result := AutoSize;
end;

function TTBADIActiveXToolWndForm.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_AxBorderStyle', tmoTiming);{$ENDIF}
  Result := Ord(AxBorderStyle);
end;

function TTBADIActiveXToolWndForm.Get_BorderWidth: Integer;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_BorderWidth', tmoTiming);{$ENDIF}
  Result := Integer(BorderWidth);
end;

function TTBADIActiveXToolWndForm.Get_Caption: WideString;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_Caption', tmoTiming);{$ENDIF}
  Result := WideString(Caption);
end;

function TTBADIActiveXToolWndForm.Get_Color: OLE_COLOR;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_Color', tmoTiming);{$ENDIF}
  Result := OLE_COLOR(Color);
end;

function TTBADIActiveXToolWndForm.Get_CurrentPPI: Integer;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_CurrentPPI', tmoTiming);{$ENDIF}
  Result := CurrentPPI;
end;

function TTBADIActiveXToolWndForm.Get_DockSite: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_DockSite', tmoTiming);{$ENDIF}
  Result := DockSite;
end;

function TTBADIActiveXToolWndForm.Get_DoubleBuffered: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_DoubleBuffered', tmoTiming);{$ENDIF}
  Result := DoubleBuffered;
end;

function TTBADIActiveXToolWndForm.Get_DropTarget: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_DropTarget', tmoTiming);{$ENDIF}
  Result := DropTarget;
end;

function TTBADIActiveXToolWndForm.Get_Enabled: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_Enabled', tmoTiming);{$ENDIF}
  Result := Enabled;
end;

function TTBADIActiveXToolWndForm.Get_ExplicitHeight: Integer;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_ExplicitHeight', tmoTiming);{$ENDIF}
  Result := ExplicitHeight;
end;

function TTBADIActiveXToolWndForm.Get_ExplicitLeft: Integer;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_ExplicitLeft', tmoTiming);{$ENDIF}
  Result := ExplicitLeft;
end;

function TTBADIActiveXToolWndForm.Get_ExplicitTop: Integer;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_ExplicitTop', tmoTiming);{$ENDIF}
  Result := ExplicitTop;
end;

function TTBADIActiveXToolWndForm.Get_ExplicitWidth: Integer;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_ExplicitWidth', tmoTiming);{$ENDIF}
  Result := ExplicitWidth;
end;

function TTBADIActiveXToolWndForm.Get_Font: IFontDisp;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_Font', tmoTiming);{$ENDIF}
  GetOleFont(Font, Result);
end;

function TTBADIActiveXToolWndForm.Get_HelpFile: WideString;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_HelpFile', tmoTiming);{$ENDIF}
  Result := WideString(HelpFile);
end;

function TTBADIActiveXToolWndForm.Get_IsDrawingLocked: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_IsDrawingLocked', tmoTiming);{$ENDIF}
  Result := IsDrawingLocked;
end;

function TTBADIActiveXToolWndForm.Get_KeyPreview: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_KeyPreview', tmoTiming);{$ENDIF}
  Result := KeyPreview;
end;

function TTBADIActiveXToolWndForm.Get_MouseInClient: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_MouseInClient', tmoTiming);{$ENDIF}
  Result := MouseInClient;
end;

function TTBADIActiveXToolWndForm.Get_ParentCustomHint: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_ParentCustomHint', tmoTiming);{$ENDIF}
  Result := ParentCustomHint;
end;

function TTBADIActiveXToolWndForm.Get_ParentDoubleBuffered: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_ParentDoubleBuffered', tmoTiming);{$ENDIF}
  Result := ParentDoubleBuffered;
end;

function TTBADIActiveXToolWndForm.Get_PixelsPerInch: Integer;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_PixelsPerInch', tmoTiming);{$ENDIF}
  Result := PixelsPerInch;
end;

function TTBADIActiveXToolWndForm.Get_PopupMode: TxPopupMode;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_PopupMode', tmoTiming);{$ENDIF}
  Result := Ord(PopupMode);
end;

function TTBADIActiveXToolWndForm.Get_PrintScale: TxPrintScale;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_PrintScale', tmoTiming);{$ENDIF}
  Result := Ord(PrintScale);
end;

function TTBADIActiveXToolWndForm.Get_RaiseOnNonMainThreadUsage: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_RaiseOnNonMainThreadUsage', tmoTiming);{$ENDIF}
  Result := RaiseOnNonMainThreadUsage;
end;

function TTBADIActiveXToolWndForm.Get_RedrawDisabled: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_RedrawDisabled', tmoTiming);{$ENDIF}
  Result := RedrawDisabled;
end;

function TTBADIActiveXToolWndForm.Get_Scaled: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_Scaled', tmoTiming);{$ENDIF}
  Result := Scaled;
end;

function TTBADIActiveXToolWndForm.Get_ScaleFactor: Single;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_ScaleFactor', tmoTiming);{$ENDIF}
  Result := ScaleFactor;
end;

function TTBADIActiveXToolWndForm.Get_ScreenSnap: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_ScreenSnap', tmoTiming);{$ENDIF}
  Result := ScreenSnap;
end;

function TTBADIActiveXToolWndForm.Get_SnapBuffer: Integer;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_SnapBuffer', tmoTiming);{$ENDIF}
  Result := SnapBuffer;
end;

function TTBADIActiveXToolWndForm.Get_StyleName: WideString;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_StyleName', tmoTiming);{$ENDIF}
  Result := WideString(StyleName);
end;

function TTBADIActiveXToolWndForm.Get_UseDockManager: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_UseDockManager', tmoTiming);{$ENDIF}
  Result := UseDockManager;
end;

function TTBADIActiveXToolWndForm.Get_Visible: WordBool;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_Visible', tmoTiming);{$ENDIF}
  Result := Visible;
end;

function TTBADIActiveXToolWndForm.Get_VisibleDockClientCount: Integer;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_VisibleDockClientCount', tmoTiming);{$ENDIF}
  Result := VisibleDockClientCount;
end;

procedure TTBADIActiveXToolWndForm._Set_Font(var Value: IFontDisp);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, '_Set_Font', tmoTiming);{$ENDIF}
  SetOleFont(Font, Value);
end;

procedure TTBADIActiveXToolWndForm.ActivateEvent(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ActivateEvent', tmoTiming);{$ENDIF}
  Align := alClient;
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TTBADIActiveXToolWndForm.AfterMonitorDpiChangedEvent(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterMonitorDpiChangedEvent', tmoTiming);{$ENDIF}
  if FEvents <> nil then FEvents.OnAfterMonitorDpiChanged(OldDPI, NewDPI);
end;

procedure TTBADIActiveXToolWndForm.BeforeMonitorDpiChangedEvent(Sender: TObject; OldDPI, NewDPI: Integer);
          
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeforeMonitorDpiChangedEvent', tmoTiming);{$ENDIF}
  if FEvents <> nil then FEvents.OnBeforeMonitorDpiChanged(OldDPI, NewDPI);
end;

procedure TTBADIActiveXToolWndForm.ClickEvent(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ClickEvent', tmoTiming);{$ENDIF}
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TTBADIActiveXToolWndForm.CreateEvent(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CreateEvent', tmoTiming);{$ENDIF}
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TTBADIActiveXToolWndForm.DblClickEvent(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'DblClickEvent', tmoTiming);{$ENDIF}
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TTBADIActiveXToolWndForm.DeactivateEvent(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'DeactivateEvent', tmoTiming);{$ENDIF}
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TTBADIActiveXToolWndForm.DestroyEvent(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'DestroyEvent', tmoTiming);{$ENDIF}
  tmUpdateBounds.Enabled := False;
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TTBADIActiveXToolWndForm.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'KeyPressEvent', tmoTiming);{$ENDIF}
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TTBADIActiveXToolWndForm.MouseEnterEvent(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'MouseEnterEvent', tmoTiming);{$ENDIF}
  if FEvents <> nil then FEvents.OnMouseEnter;
end;

procedure TTBADIActiveXToolWndForm.MouseLeaveEvent(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'MouseLeaveEvent', tmoTiming);{$ENDIF}
  if FEvents <> nil then FEvents.OnMouseLeave;
end;

procedure TTBADIActiveXToolWndForm.PaintEvent(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'PaintEvent', tmoTiming);{$ENDIF}
  //: @debug CodeSite.Send('Form', Self.BoundsRect);
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TTBADIActiveXToolWndForm.Set_AlignWithMargins(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_AlignWithMargins', tmoTiming);{$ENDIF}
  AlignWithMargins := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_AutoScroll(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_AutoScroll', tmoTiming);{$ENDIF}
  AutoScroll := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_AutoSize(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_AutoSize', tmoTiming);{$ENDIF}
  AutoSize := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_AxBorderStyle', tmoTiming);{$ENDIF}
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_BorderWidth(Value: Integer);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_BorderWidth', tmoTiming);{$ENDIF}
  BorderWidth := TBorderWidth(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_Caption(const Value: WideString);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_Caption', tmoTiming);{$ENDIF}
  Caption := TCaption(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_Color(Value: OLE_COLOR);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_Color', tmoTiming);{$ENDIF}
  Color := TColor(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_DockSite(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_DockSite', tmoTiming);{$ENDIF}
  DockSite := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_DoubleBuffered(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_DoubleBuffered', tmoTiming);{$ENDIF}
  DoubleBuffered := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_DropTarget(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_DropTarget', tmoTiming);{$ENDIF}
  DropTarget := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_Enabled(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_Enabled', tmoTiming);{$ENDIF}
  Enabled := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_Font(const Value: IFontDisp);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_Font', tmoTiming);{$ENDIF}
  SetOleFont(Font, Value);
end;

procedure TTBADIActiveXToolWndForm.Set_HelpFile(const Value: WideString);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_HelpFile', tmoTiming);{$ENDIF}
  HelpFile := string(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_KeyPreview(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_KeyPreview', tmoTiming);{$ENDIF}
  KeyPreview := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_ParentCustomHint(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_ParentCustomHint', tmoTiming);{$ENDIF}
  ParentCustomHint := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_ParentDoubleBuffered(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_ParentDoubleBuffered', tmoTiming);{$ENDIF}
  ParentDoubleBuffered := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_PixelsPerInch(Value: Integer);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_PixelsPerInch', tmoTiming);{$ENDIF}
  PixelsPerInch := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_PopupMode(Value: TxPopupMode);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_PopupMode', tmoTiming);{$ENDIF}
  PopupMode := TPopupMode(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_PrintScale(Value: TxPrintScale);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_PrintScale', tmoTiming);{$ENDIF}
  PrintScale := TPrintScale(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_RaiseOnNonMainThreadUsage(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_RaiseOnNonMainThreadUsage', tmoTiming);{$ENDIF}
  RaiseOnNonMainThreadUsage := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_Scaled(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_Scaled', tmoTiming);{$ENDIF}
  Scaled := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_ScreenSnap(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_ScreenSnap', tmoTiming);{$ENDIF}
  ScreenSnap := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_SnapBuffer(Value: Integer);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_SnapBuffer', tmoTiming);{$ENDIF}
  SnapBuffer := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_StyleName(const Value: WideString);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_StyleName', tmoTiming);{$ENDIF}
  StyleName := string(Value);
end;

procedure TTBADIActiveXToolWndForm.Set_UseDockManager(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_UseDockManager', tmoTiming);{$ENDIF}
  UseDockManager := Value;
end;

procedure TTBADIActiveXToolWndForm.Set_Visible(Value: WordBool);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_Visible', tmoTiming);{$ENDIF}
  Visible := Value;
end;

procedure TTBADIActiveXToolWndForm.UpdateBounds;

Var
  WindowInfo : TWindowInfo;
  
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateBounds', tmoTiming);{$ENDIF}
  //: @bug Need to understand the qwindow hierarchy as I think there is another window above the parent.
  //CodeSite.Send('Self.BoundsRect.Before', Self.BoundsRect);
  GetWindowInfo(ParentWindow, WindowInfo);
  //CodeSite.Send('ClientRect.Width', WindowInfo.rcClient.Width);
  //CodeSite.Send('ClientRect.Height', WindowInfo.rcClient.Height);
  If (Width <> WindowInfo.rcClient.Width) Or (Height <> WindowInfo.rcClient.Height) Then
    Begin
      SetWindowPos(
        WindowHandle,
        HWND_TOP,
        0,
        0,
        WindowInfo.rcClient.Width,
        WindowInfo.rcClient.Height,
        SWP_NOZORDER + SWP_NOACTIVATE
      );
      //CodeSite.Send(csmNote, 'SetWindowPos', ClientRect);
    End;                                    
  //CodeSite.Send('Self.BoundsRect.After', Self.BoundsRect);
end;

Procedure TTBADIActiveXToolWndForm.WMMove(Var M: TWMMove);
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WMMove', tmoTiming);{$ENDIF}
  Inherited;
End;

Procedure TTBADIActiveXToolWndForm.WMMoving(Var M: TWMMoving);
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WMMoving', tmoTiming);{$ENDIF}
  Inherited;
End;

Procedure TTBADIActiveXToolWndForm.WMResize(var M : TWMSize);

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WMResize', tmoTiming);{$ENDIF}
  Inherited;
  //CodeSite.Send('RESIZE.Width', M.Width);
  //CodeSite.Send('RESIZE.Height', M.Height);
  UpdateBounds;
end;

Procedure TTBADIActiveXToolWndForm.WMResizing(Var M: TMessage);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WMResizing', tmoTiming);{$ENDIF}
  Inherited;
End;

Procedure TTBADIActiveXToolWndForm.WMWindowPosChanged(Var M: TMessage);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WMWindowPosChanged', tmoTiming);{$ENDIF}
  Inherited;
End;

function TTBADIActiveXToolWndForm.Get_VCLFormRef: TBADIActiveXToolWndForm;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Get_VCLFormRef', tmoTiming);{$ENDIF}
  Result := Self;
end;

procedure TTBADIActiveXToolWndForm.Set_VCLFormRef(const Value: TBADIActiveXToolWndForm);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Set_VCLFormRef', tmoTiming);{$ENDIF}
end;

procedure TTBADIActiveXToolWndForm.tmUpdateBoundsTimer(Sender: TObject);
begin
  UpdateBounds;
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
