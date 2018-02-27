unit AddInDesignerObjects_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.88  $
// File generated on 10/28/2000 1:19:32 PM from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Program Files\Common Files\Designer\MSADDNDR.DLL (1)
// IID\LCID: {AC0714F2-3D04-11D1-AE7D-00A0C90F26F4}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\StdOle2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AddInDesignerObjectsMajorVersion = 1;
  AddInDesignerObjectsMinorVersion = 0;

  LIBID_AddInDesignerObjects: TGUID = '{AC0714F2-3D04-11D1-AE7D-00A0C90F26F4}';

  IID_IAddinDesigner: TGUID = '{AC0714F3-3D04-11D1-AE7D-00A0C90F26F4}';
  IID_IAddinInstance: TGUID = '{AC0714F4-3D04-11D1-AE7D-00A0C90F26F4}';
  IID__IDTExtensibility2: TGUID = '{B65AD801-ABAF-11D0-BB8B-00A0C90F2744}';
  CLASS_AddinDesigner: TGUID = '{AC0714F6-3D04-11D1-AE7D-00A0C90F26F4}';
  CLASS_AddinInstance: TGUID = '{AC0714F7-3D04-11D1-AE7D-00A0C90F26F4}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum ext_ConnectMode
type
  ext_ConnectMode = TOleEnum;
const
  ext_cm_AfterStartup = $00000000;
  ext_cm_Startup = $00000001;
  ext_cm_External = $00000002;
  ext_cm_CommandLine = $00000003;

// Constants for enum ext_DisconnectMode
type
  ext_DisconnectMode = TOleEnum;
const
  ext_dm_HostShutdown = $00000000;
  ext_dm_UserClosed = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IAddinDesigner = interface;
  IAddinDesignerDisp = dispinterface;
  IAddinInstance = interface;
  IAddinInstanceDisp = dispinterface;
  _IDTExtensibility2 = interface;
  _IDTExtensibility2Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  AddinDesigner = IAddinDesigner;
  AddinInstance = IAddinInstance;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPSafeArray1 = ^PSafeArray; {*}

  IDTExtensibility2 = _IDTExtensibility2; 

// *********************************************************************//
// Interface: IAddinDesigner
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {AC0714F3-3D04-11D1-AE7D-00A0C90F26F4}
// *********************************************************************//
  IAddinDesigner = interface(IDispatch)
    ['{AC0714F3-3D04-11D1-AE7D-00A0C90F26F4}']
  end;

// *********************************************************************//
// DispIntf:  IAddinDesignerDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {AC0714F3-3D04-11D1-AE7D-00A0C90F26F4}
// *********************************************************************//
  IAddinDesignerDisp = dispinterface
    ['{AC0714F3-3D04-11D1-AE7D-00A0C90F26F4}']
  end;

// *********************************************************************//
// Interface: IAddinInstance
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {AC0714F4-3D04-11D1-AE7D-00A0C90F26F4}
// *********************************************************************//
  IAddinInstance = interface(IDispatch)
    ['{AC0714F4-3D04-11D1-AE7D-00A0C90F26F4}']
  end;

// *********************************************************************//
// DispIntf:  IAddinInstanceDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {AC0714F4-3D04-11D1-AE7D-00A0C90F26F4}
// *********************************************************************//
  IAddinInstanceDisp = dispinterface
    ['{AC0714F4-3D04-11D1-AE7D-00A0C90F26F4}']
  end;

// *********************************************************************//
// Interface: _IDTExtensibility2
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B65AD801-ABAF-11D0-BB8B-00A0C90F2744}
// *********************************************************************//
  _IDTExtensibility2 = interface(IDispatch)
    ['{B65AD801-ABAF-11D0-BB8B-00A0C90F2744}']
    procedure OnConnection(const Application: IDispatch; ConnectMode: ext_ConnectMode; 
                           const AddInInst: IDispatch; var custom: PSafeArray); safecall;
    procedure OnDisconnection(RemoveMode: ext_DisconnectMode; var custom: PSafeArray); safecall;
    procedure OnAddInsUpdate(var custom: PSafeArray); safecall;
    procedure OnStartupComplete(var custom: PSafeArray); safecall;
    procedure OnBeginShutdown(var custom: PSafeArray); safecall;
  end;

// *********************************************************************//
// DispIntf:  _IDTExtensibility2Disp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B65AD801-ABAF-11D0-BB8B-00A0C90F2744}
// *********************************************************************//
  _IDTExtensibility2Disp = dispinterface
    ['{B65AD801-ABAF-11D0-BB8B-00A0C90F2744}']
    procedure OnConnection(const Application: IDispatch; ConnectMode: ext_ConnectMode; 
                           const AddInInst: IDispatch; var custom: {??PSafeArray} OleVariant); dispid 1;
    procedure OnDisconnection(RemoveMode: ext_DisconnectMode; var custom: {??PSafeArray} OleVariant); dispid 2;
    procedure OnAddInsUpdate(var custom: {??PSafeArray} OleVariant); dispid 3;
    procedure OnStartupComplete(var custom: {??PSafeArray} OleVariant); dispid 4;
    procedure OnBeginShutdown(var custom: {??PSafeArray} OleVariant); dispid 5;
  end;

// *********************************************************************//
// The Class CoAddinDesigner provides a Create and CreateRemote method to          
// create instances of the default interface IAddinDesigner exposed by              
// the CoClass AddinDesigner. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAddinDesigner = class
    class function Create: IAddinDesigner;
    class function CreateRemote(const MachineName: string): IAddinDesigner;
  end;

// *********************************************************************//
// The Class CoAddinInstance provides a Create and CreateRemote method to          
// create instances of the default interface IAddinInstance exposed by              
// the CoClass AddinInstance. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAddinInstance = class
    class function Create: IAddinInstance;
    class function CreateRemote(const MachineName: string): IAddinInstance;
  end;

implementation

uses ComObj;

class function CoAddinDesigner.Create: IAddinDesigner;
begin
  Result := CreateComObject(CLASS_AddinDesigner) as IAddinDesigner;
end;

class function CoAddinDesigner.CreateRemote(const MachineName: string): IAddinDesigner;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AddinDesigner) as IAddinDesigner;
end;

class function CoAddinInstance.Create: IAddinInstance;
begin
  Result := CreateComObject(CLASS_AddinInstance) as IAddinInstance;
end;

class function CoAddinInstance.CreateRemote(const MachineName: string): IAddinInstance;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AddinInstance) as IAddinInstance;
end;

end.
