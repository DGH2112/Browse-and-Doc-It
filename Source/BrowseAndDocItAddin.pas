(**
  
  This module defines a set of classes for interfacing with the VBE IDE and
  handling the COM interfaces required.

  @Author  David Hoyle
  @Version 1.850
  @Date    10 Sep 2023

**)
unit BrowseAndDocItAddin;

interface

uses
  System.Win.ComObj,
  System.Win.StdVCL,
  Winapi.Windows,
  Winapi.ActiveX,
  {$IFDEF WIN32}
  BrowseAndDocItVBEIDE_TLB,
  {$ELSE}
  BrowseAndDocItVBEIDE64_TLB,
  {$ENDIF}
  AddInDesignerObjects_TLB,
  Office2000_TLB,
  BADI.IDETools,
  BADI.VBEIDE.ActiveForm;

type
  (** A the add-in automation object **)
  TTVBDoc50Addin = class(TAutoObject, IBrowseAndDocItVBEIDETypeLib, IDTExtensibility2)
  Private
    FVBEIDE : TIDETools;
    FToolWindow : ITBADIActiveXToolWndForm;
  protected
    { Protected declarations }
    procedure OnConnection(const Application_: IDispatch;
      ConnectMode: ext_ConnectMode; const AddInInst: IDispatch;
      var custom: PSafeArray); safecall;
    procedure OnDisconnection(RemoveMode: ext_DisconnectMode;
      var custom: PSafeArray); safecall;
    procedure OnAddInsUpdate(var custom: PSafeArray); safecall;
    procedure OnStartupComplete(var custom: PSafeArray); safecall;
    procedure OnBeginShutdown(var custom: PSafeArray); safecall;
  end;

  (** A factory for creating the add-in. **)
  TOfficeAddInFactory = Class(TAutoObjectFactory)
  Private
    FFriendlyName : String;
    FLoadBehaviour : Integer;
    Procedure ReallyDeleteRegKey(Const KeyName : String);
  Protected
    Procedure RegisterAddIn(Const KeyName : String);
  Public
    Constructor Create(ComServer : TComServerObject; AutoClass : TAutoClass;
      Const ClassID : TGUID; Instancing : TClassInstancing;
      ThreadingModel : TThreadingModel; FriendlyName : String;
      LoadBehaviour : Integer);
    Procedure UpdateRegistry(Register : Boolean); Override;
  End;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Win.Registry,
  Vcl.Forms,
  Vcl.Dialogs,
  ComServ,
  VBIDE_TLB,
  BADI.Functions,
  CodeSiteLogging;

Const
  (** A constant to define the load behaviour of the Add-in. **)
  iLoadBehaviour = 3;

ResourceString
  (** A resource string to provide a name/title for the add-in. **)
  strBrowseAndDocItVBEIDETools = 'Browse and Doc It VBE IDE Tools';

(**

  This is an on add-in insert update event handler for the com interface.

  @precon  None.
  @postcon This needs to be implemented for the COM DLL to work but is not
           handled in this application.

  @nocheck ExceptionEating
  @nohint  custom

  @param   custom as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnAddInsUpdate(var custom: PSafeArray);

Const
  strOnAddInsUpdate = 'OnAddInsUpdate: ';

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'OnAddInsUpdate', tmoTiming);{$ENDIF}
  Try
    // Do Nothing;
  Except
    On E : Exception Do DisplayException(E);
  End;
end;

(**

  This is an on begin shutdown event handler for the com interface.

  @precon  None.
  @postcon This needs to be implemented for the COM DLL to work but is not
           handled in this application.

  @nocheck ExceptionEating
  @nohint  custom

  @param   custom as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnBeginShutdown(var custom: PSafeArray);

Const
  strOnBeginShutDown = 'OnBeginShutDown: ';

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'OnBeginShutdown', tmoTiming);{$ENDIF}
  Try
    // Do nothing;
  Except
    On E : Exception Do DisplayException(E);
  End;
end;

(**

  This is an on connect event handler for the com interface.

  @precon  None.
  @postcon This event creates an instance of the Browse And Doc It IDE Interface.

  @nocheck MissingCONSTInParam ExceptionEating
  @nohint  ConnectMode custom AddInInst

  @param   Application_ as an IDispatch as a constant
  @param   ConnectMode  as an ext_ConnectMode
  @param   AddInInst    as an IDispatch as a constant
  @param   custom       as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnConnection(
  const Application_: IDispatch; ConnectMode: ext_ConnectMode;
  const AddInInst: IDispatch; var custom: PSafeArray);

Var
  Wnd : VBIDE_TLB.Window_;
  DocObj : IDispatch;
  
Const
  strOnConnection = 'OnConnection: ';

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'OnConnection', tmoTiming);{$ENDIF}
  Try
    FVBEIDE := TIDETools.Create(Application_ As VBIDE_TLB.VBE);
    Wnd := (Application_ As VBIDE_TLB.VBE).Windows.CreateToolWindow(
      AddInInst As AddIn,
      {$IFDEF Win32}
      'BrowseAndDocItVBEIDE.TBADIActiveXToolWndForm',
      {$ELSE}
      'BrowseAndDocItVBEIDE64.TBADIActiveXToolWndForm',
      {$ENDIF}
      'Browse and Doc It',
      'BrowseAndDocIt.GuidPosition',
      DocObj
    );
    If Supports(DocObj, ITBADIActiveXToolWndForm, FToolWindow) Then
      FVBEIDE.CreateModuleExplorer(FToolWindow, Wnd);
  Except
    On E : Exception Do
      Begin
        DisplayException(E);
        CodeSite.SendException(E);
      End;
  End;
end;

(**

  This is an on disconnect event handler for the com interface.

  @precon  None.
  @postcon This method frees the memory used by the Browse and Doc It IDE
           interface.

  @nocheck MissingCONSTInParam ExceptionEating
  @nohint  RemoveMode custom

  @param   RemoveMode as an ext_DisconnectMode
  @param   custom     as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnDisconnection(RemoveMode: ext_DisconnectMode; var custom: PSafeArray);

Const
  strOnDisconnection = 'OnDisconnection: ';

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'OnDisconnection', tmoTiming);{$ENDIF}
  Try
    //: @note THIS SHOULD NOT BE REQUIRED ON AN INTERFACED OBJECT BUT IS REQUIRED HERE
    FVBEIDE.DestroyModuleExplorer();
    FVBEIDE.Free;
  Except
    On E : Exception Do DisplayException(E);
  End;
end;

(**

  This is an on Start-up Complete event handler for the com interface.

  @precon  None.
  @postcon This needs to be implemented for the COM DLL to work but is not
           handled in this application.

  @nocheck ExceptionEating
  @nohint  custom

  @param   custom as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnStartupComplete(var custom: PSafeArray);

Const
  strOnStartupComplete = 'OnStartupComplete: ';

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'OnStartupComplete', tmoTiming);{$ENDIF}
  Try
    // Do Nothing;
  Except
    On E : Exception Do DisplayException(E);
  End;
end;

{ TOfficeAddInFactory }

(**

  This is a constructor for the TOfficeAddInFactory class.

  @precon  None.
  @postcon Initialises the COM DLL interface.

  @nocheck MissingCONSTInParam

  @param   ComServer      as a TComServerObject
  @param   AutoClass      as a TAutoClass
  @param   ClassID        as a TGUID as a constant
  @param   Instancing     as a TClassInstancing
  @param   ThreadingModel as a TThreadingModel
  @param   FriendlyName   as a String
  @param   LoadBehaviour  as an Integer

**)
constructor TOfficeAddInFactory.Create(ComServer: TComServerObject;
  AutoClass: TAutoClass; const ClassID: TGUID;
  Instancing: TClassInstancing; ThreadingModel: TThreadingModel;
  FriendlyName: String; LoadBehaviour: Integer);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create(ComServer, AutoClass, CLassID, Instancing, ThreadingModel);
  FFriendlyName := FriendlyName;
  FLoadBehaviour := LoadBehaviour;
end;

(**

  This method deletes the register keys not needed anymore.

  @precon  None.
  @postcon Deletes the register keys not needed anymore.

  @param   KeyName as a String as a constant

**)
procedure TOfficeAddInFactory.ReallyDeleteRegKey(const KeyName: String);

Var
  R : TRegistry;
  Values : TStringList;
  i : Integer;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ReallyDeleteRegKey', tmoTiming);{$ENDIF}
  Values := TStringList.Create;
  Try
    R := TRegistry.Create;
    Try
      R.RootKey := HKEY_CURRENT_USER;
      If R.OpenKey(KeyName, False) Then
        Begin
          R.GetValueNames(Values);
          For i := 0 To Values.Count - 1 Do
            R.DeleteValue(Values[i]);
          R.CloseKey;
          R.DeleteKey(KeyName);
        End;
    Finally
      R.Free;
    End;
  Finally
    Values.Free;
  End;
end;

(**

  This method registers the COM DLL add-in.

  @precon  None.
  @postcon Registers the COM DLL add-in.

  @param   KeyName as a String as a constant

**)
procedure TOfficeAddInFactory.RegisterAddIn(const KeyName: String);

Const
  strExceptionInRegisterAddIn = 'Exception in RegisterAddIn';
  strFriendlyName = 'FriendlyName';
  strDescription = 'Description';
  strLoadBehavior = 'LoadBehavior';
  strCommandLineSafe = 'CommandLineSafe';

Var
  R : TRegistry;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'RegisterAddIn', tmoTiming);{$ENDIF}
  R := TRegistry.Create;
  Try
    R.RootKey := HKEY_CURRENT_USER;
    If Not R.OpenKey(KeyName, True) Then
      Begin
        DisplayException(strExceptionInRegisterAddIn, []);
        Exit;
      End;
    R.WriteString(strFriendlyName, FFriendlyName);
    R.WriteString(strDescription, Description);
    R.WriteInteger(strLoadBehavior, FLoadBehaviour);
    R.WriteInteger(strCommandLineSafe, 0);
  Finally
    R.Free;
  End;
end;

(**

  This method updates the registration of this COM DLL based on the parameters
  passed to REGSVR32.

  @precon  None.
  @postcon This registers or un-registers this COM DLL.

  @nocheck MissingCONSTInParam

  @param   Register as a Boolean

**)
procedure TOfficeAddInFactory.UpdateRegistry(Register: Boolean);

Const
  {$IFDEF WIN32}
  AddInKey = '\Software\Microsoft\VBA\VBE\%d.0\AddIns\%s';
  {$ENDIF}
  {$IFDEF WIN64}
  AddInKey = '\Software\Microsoft\VBA\VBE\%d.0\AddIns64\%s';
  {$ENDIF}
  iLowVBEVerNo = 5;
  iHighVBEVerNo = 6;

Var
  i : Integer;
  CurrentAddInKey : String;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateRegistry', tmoTiming);{$ENDIF}
  Inherited UpdateRegistry(Register);
  For i := iLowVBEVerNo To iHighVBEVerNo Do // Version of the VBE IDE
    Begin
      CurrentAddInKey := Format(AddInKey, [i, ProgID]);
      If Register Then
        RegisterAddIn(CurrentAddInKey)
      Else
        ReallyDeleteRegKey(CurrentAddInKey);
    End;
end;

initialization
  TOfficeAddinFactory.Create(ComServer, TTVBDoc50Addin, Class_BrowseAndDocItVBEIDETypeLib,
    ciMultiInstance, tmApartment, strBrowseAndDocItVBEIDETools, iLoadBehaviour);
end.


