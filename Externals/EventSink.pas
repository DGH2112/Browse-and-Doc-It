(**

  This module contains a class to provide COM event handling adapter between
  the COM events and the VCL event.

  @Version 1.0
  @Author  Steve Trefethen
  @Date    28 Jan 2010

**)
unit EventSink;

interface

uses Windows, ActiveX, Office2000_TLB;

type
  (** An event type for the VCL events to be attached to the COM events. **)
  TClickProc = procedure (const Ctrl: CommandBarButton;
    var CancelDefault: WordBool) of object;

  (** A class to encapulate the VCL event with the COM event. **)
  TEventSink = class(TObject, IUnknown, IDispatch)
  Private
    FClickProc: TClickProc;
  Public
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    constructor Create(ClickProc: TClickProc);
  end;

implementation


Uses
  {$IFDEF EUREKALOG} ExceptionLog, {$ENDIF} SysUtils, Dialogs;


{ TEventSink }

(**

  This is the constructor method for the TEventSink class.

  @precon  None.
  @postcon Attaches a VCL eventto this COM event handler.

  @param   ClickProc as a TClickProc

**)
constructor TEventSink.Create(ClickProc: TClickProc);
begin
  FClickProc := ClickProc;
  inherited Create;
end;

{ TEventSink.IUnknown }

(**

  Implementation of IUnknowns AddRef method.

  @precon  None.
  @postcon Implementation of IUnknowns AddRef method.

  @return  an Integer

**)
function TEventSink._AddRef: Integer;
begin
  // No need to implement, since lifetime is tied to add-in
  Result := 2;
end;

(**

  Implementation of IUnknowns Release method.

  @precon  None.
  @postcon Implementation of IUnknowns Release method.

  @return  an Integer

**)
function TEventSink._Release: Integer;
begin
  // No need to implement, since lifetime is tied to add-in
  Result := 1;
end;

(**

  Implementation of IUnknowns QueryInterface method.

  @precon  None.
  @postcon Implementation of IUnknowns QueryInterface method.

  @param   IID as a TGUID as a constant
  @param   Obj as   @return  a HResult

**)
function TEventSink.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  // First look for my own implementation of an interface
  // (I implement IUnknown and IDispatch).
  if GetInterface(IID, Obj) then
    Result := S_OK
  // Next, if they are looking for outgoing interface, recurse to return
  // our IDispatch pointer.
  else if IsEqualIID(IID, DIID__CommandBarButtonEvents) then
    Result := QueryInterface(IDispatch, Obj)
  // For everything else, return an error.
  else
    Result := E_NOINTERFACE;
end;

{ TEventSink.IDispatch }

(**

  Implementation of IDispatch GetIDsOfNames method.

  @precon  None.
  @postcon Implementation of IDispatch GetIDsOfNames method.

  @param   IID       as a TGUID as a constant
  @param   Names     as a Pointer
  @param   NameCount as an Integer
  @param   LocaleID  as an Integer
  @param   DispIDs   as a Pointer
  @return  a HResult

**)
function TEventSink.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

(**

  Implementation of IDispatch GetTypeInfo method.

  @precon  None.
  @postcon Implementation of IDispatch GetTypeInfo method.

  @param   Index    as an Integer
  @param   LocaleID as an Integer
  @param   TypeInfo as   @return  a HResult

**)
function TEventSink.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

(**

  Implementation of IDispatch GetTypeInfoCount method.

  @precon  None.
  @postcon Implementation of IDispatch GetTypeInfoCount method.

  @param   Count as an Integer as an out parameter
  @return  a HResult

**)
function TEventSink.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

(**

  Implementation of IDispatch Invoke method.

  @precon  None.
  @postcon Implementation of IDispatch Invoke method.

  @param   DispID    as an Integer
  @param   IID       as a TGUID as a constant
  @param   LocaleID  as an Integer
  @param   Flags     as a Word
  @param   Params
  @param   VarResult as a Pointer
  @param   ExcepInfo as a Pointer
  @param   ArgErr    as a Pointer
  @return  a HResult

**)
function TEventSink.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;

var
  DispParams: PVariantArgList;

Begin
  DispParams := TDispParams(Params).rgvarg;
  // Pass click event back to add-in
  if DispID = 1 then
    Try
      FClickProc(CommandBarButton(DispParams^[0].dispVal), DispParams^[1].pBool^);
    Except
      On E : Exception Do
        Begin
          {$IFDEF EUREKALOG}
          If Not StandardEurekaNotify(GetLastExceptionObject,
            GetLastExceptionAddress) Then
          {$ENDIF}
            ShowMessage('Exception: ' + E.Message);
        End;
    End;
  Result := S_OK;
end;

end.
