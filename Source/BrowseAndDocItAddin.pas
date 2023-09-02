(**
  
  This module defines a set of classes for interfacing with the VBE IDE and
  handling the COM interfaces required.

  @Author  David Hoyle
  @Version 1.026
  @Date    02 Sep 2023

**)
unit BrowseAndDocItAddin;

interface

uses
  System.Win.ComObj,
  System.Win.StdVCL,
  Winapi.Windows,
  Winapi.ActiveX,
  BrowseAndDocItVBEIDE_TLB,
  AddInDesignerObjects_TLB,
  Office2000_TLB,
  BADI.IDETools;

type
  (** A the addin automation object **)
  TTVBDoc50Addin = class(TAutoObject, IBrowseAndDocItVBEIDETypeLib, IDTExtensibility2)
  Private
    FVBEIDE : TIDETools;
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

  (** A factory for creating the addin. **)
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
  Vcl.Dialogs,
  ComServ,
  VBIDE_TLB,
  BADI.Functions;

{ TTDGHVBEIDEToosl50Addin }

(**

  This is an on addin insert update event handler for the com interface.

  @precon  None.
  @postcon This needs to be implemented for the COM DLL to work but is not
           handled in this application.

  @param   custom as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnAddInsUpdate(var custom: PSafeArray);
begin
  Try
    // Do Nothing;
  Except
    On E : Exception Do DisplayException('OnAddInsUpdate: ' + E.Message);
  End;
end;

(**

  This is an on begin shutdown event handler for the com interface.

  @precon  None.
  @postcon This needs to be implemented for the COM DLL to work but is not
           handled in this application.

  @param   custom as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnBeginShutdown(var custom: PSafeArray);
begin
  Try
    // Do nothing;
  Except
    On E : Exception Do DisplayException('OnBeginShutDown: ' + E.Message);
  End;
end;

(**

  This is an on connect event handler for the com interface.

  @precon  None.
  @postcon This event creates an instance of the Browse And Doc It IDE Interface.

  @param   Application_ as an IDispatch as a constant
  @param   ConnectMode  as an ext_ConnectMode
  @param   AddInInst    as an IDispatch as a constant
  @param   custom       as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnConnection(
  const Application_: IDispatch; ConnectMode: ext_ConnectMode;
  const AddInInst: IDispatch; var custom: PSafeArray);

begin
  Try
    FVBEIDE := TIDETools.Create(Application_ As VBIDE_TLB.VBE)
  Except
    On E : Exception Do DisplayException('OnConnection: ' + E.Message);
  End;
end;

(**

  This is an on disconnect event handler for the com interface.

  @precon  None.
  @postcon This method frees the memory used by the Browse and Doc It IDE
           interface.

  @param   RemoveMode as an ext_DisconnectMode
  @param   custom     as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnDisconnection(
  RemoveMode: ext_DisconnectMode; var custom: PSafeArray);
begin
  Try
    FVBEIDE.Free;
  Except
    On E : Exception Do DisplayException('OnDisconnection: ' + E.Message);
  End;
end;

(**

  This is an on Startup Complete event handler for the com interface.

  @precon  None.
  @postcon This needs to be implemented for the COM DLL to work but is not
           handled in this application.

  @param   custom as a PSafeArray as a reference

**)
procedure TTVBDoc50Addin.OnStartupComplete(
  var custom: PSafeArray);
begin
  Try
    // Do Nothing;
  Except
    On E : Exception Do DisplayException('OnStartupComplete: ' + E.Message);
  End;
end;

{ TOfficeAddInFactory }

(**

  This is a constructor for the TOfficeAddInFactory class.

  @precon  None.
  @postcon Initialises the COM DLL interface.

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
  Values := TStringList.Create;
  Try
    R := TRegistry.Create;
    Try
      R.RootKey := HKEY_LOCAL_MACHINE;
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

  This method registers the COM DLL addin.

  @precon  None.
  @postcon Registers the COM DLL addin.

  @param   KeyName as a String as a constant

**)
procedure TOfficeAddInFactory.RegisterAddIn(const KeyName: String);

Var
  R : TRegistry;

begin
  R := TRegistry.Create;
  Try
    R.RootKey := HKEY_LOCAL_MACHINE;
    If Not R.OpenKey(KeyName, True) Then
      Begin
        DisplayException('Exception in RegisterAddIn');
        Exit;
      End;
    R.WriteString('FriendlyName', FFriendlyName);
    R.WriteString('Description', Description);
    R.WriteInteger('LoadBehavior', FLoadBehaviour);
    R.WriteInteger('CommandLineSafe', 0);
  Finally
    R.Free;
  End;
end;

(**

  This method updates the registration of this COM DLL based on the parameters
  passed to REGSVR32.

  @precon  None.
  @postcon This registers or unregisters this COM DLL.

  @param   Register as a Boolean

**)
procedure TOfficeAddInFactory.UpdateRegistry(Register: Boolean);

Const
  AddInKey = '\Software\Microsoft\VBA\VBE\%d.0\AddIns\%s';

Var
  i : Integer;
  CurrentAddInKey : String;

begin
  Inherited UpdateRegistry(Register);
  For i := 5 To 6 Do // Version of the VBE IDE
    Begin
      CurrentAddInKey := Format(AddInKey, [i, ProgID]);
      If Register Then
        RegisterAddIn(CurrentAddInKey)
      Else
        ReallyDeleteRegKey(CurrentAddInKey);
    End;
end;

(** This code initialises the COM DLL. **)
initialization
  TOfficeAddinFactory.Create(ComServer, TTVBDoc50Addin,
    Class_BrowseAndDocItVBEIDETypeLib, ciMultiInstance, tmApartment,
    'Browse and Doc It VBE IDE Tools', 3);
end.
