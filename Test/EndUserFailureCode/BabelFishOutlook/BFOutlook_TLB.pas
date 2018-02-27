unit BFOutlook_TLB;

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

// PASTLWTR : 1.2
// File generated on 19-1-2007 12:34:26 from Type Library described below.

// ************************************************************************  //
// Type Lib: D:\Dev\BabelFishOutlook\BFOutlook.tlb (1)
// LIBID: {32105D5B-444A-4D63-A355-1A90981B997B}
// LCID: 0
// Helpfile: 
// HelpString: BFOutlook Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  BFOutlookMajorVersion = 1;
  BFOutlookMinorVersion = 0;

  LIBID_BFOutlook: TGUID = '{32105D5B-444A-4D63-A355-1A90981B997B}';

  IID_IBabelFish: TGUID = '{7F239832-3F61-4989-9AAA-35507903C9E2}';
  CLASS_BabelFish: TGUID = '{A30175FA-68A4-4D8D-8D49-3ED2AB4881AE}';
  IID_IToolsOptionsPage: TGUID = '{8E6DCAC0-4606-424D-89D9-FDBA372EC908}';
  CLASS_ToolsOptionsPage: TGUID = '{8AF3CAC3-153F-42B8-8A01-C8E6A38DD0BA}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IBabelFish = interface;
  IBabelFishDisp = dispinterface;
  IToolsOptionsPage = interface;
  IToolsOptionsPageDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  BabelFish = IBabelFish;
  ToolsOptionsPage = IToolsOptionsPage;


// *********************************************************************//
// Interface: IBabelFish
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7F239832-3F61-4989-9AAA-35507903C9E2}
// *********************************************************************//
  IBabelFish = interface(IDispatch)
    ['{7F239832-3F61-4989-9AAA-35507903C9E2}']
  end;

// *********************************************************************//
// DispIntf:  IBabelFishDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7F239832-3F61-4989-9AAA-35507903C9E2}
// *********************************************************************//
  IBabelFishDisp = dispinterface
    ['{7F239832-3F61-4989-9AAA-35507903C9E2}']
  end;

// *********************************************************************//
// Interface: IToolsOptionsPage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E6DCAC0-4606-424D-89D9-FDBA372EC908}
// *********************************************************************//
  IToolsOptionsPage = interface(IDispatch)
    ['{8E6DCAC0-4606-424D-89D9-FDBA372EC908}']
  end;

// *********************************************************************//
// DispIntf:  IToolsOptionsPageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E6DCAC0-4606-424D-89D9-FDBA372EC908}
// *********************************************************************//
  IToolsOptionsPageDisp = dispinterface
    ['{8E6DCAC0-4606-424D-89D9-FDBA372EC908}']
  end;

// *********************************************************************//
// The Class CoBabelFish provides a Create and CreateRemote method to          
// create instances of the default interface IBabelFish exposed by              
// the CoClass BabelFish. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoBabelFish = class
    class function Create: IBabelFish;
    class function CreateRemote(const MachineName: string): IBabelFish;
  end;

// *********************************************************************//
// The Class CoToolsOptionsPage provides a Create and CreateRemote method to          
// create instances of the default interface IToolsOptionsPage exposed by              
// the CoClass ToolsOptionsPage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoToolsOptionsPage = class
    class function Create: IToolsOptionsPage;
    class function CreateRemote(const MachineName: string): IToolsOptionsPage;
  end;

implementation

uses ComObj;

class function CoBabelFish.Create: IBabelFish;
begin
  Result := CreateComObject(CLASS_BabelFish) as IBabelFish;
end;

class function CoBabelFish.CreateRemote(const MachineName: string): IBabelFish;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_BabelFish) as IBabelFish;
end;

class function CoToolsOptionsPage.Create: IToolsOptionsPage;
begin
  Result := CreateComObject(CLASS_ToolsOptionsPage) as IToolsOptionsPage;
end;

class function CoToolsOptionsPage.CreateRemote(const MachineName: string): IToolsOptionsPage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ToolsOptionsPage) as IToolsOptionsPage;
end;

end.
