unit BrowseAndDocItVBEIDE_TLB;

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
// File generated on 22/Mar/2009 12:32:38 from Type Library described below.

// ************************************************************************  //
// Type Lib: E:\HoylD\Borland Studio Projects\IDE Addins\BrowseAndDocIt\DLL\BrowseAndDocItVBEIDE.tlb (1)
// LIBID: {0E55B3B9-B861-4FF5-B776-30AA2E1EA00B}
// LCID: 0
// Helpfile: 
// HelpString: BrowseAndDocItVBEIDE Library
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
  BrowseAndDocItVBEIDEMajorVersion = 1;
  BrowseAndDocItVBEIDEMinorVersion = 0;

  LIBID_BrowseAndDocItVBEIDE: TGUID = '{0E55B3B9-B861-4FF5-B776-30AA2E1EA00B}';

  IID_IBrowseAndDocItVBEIDETypeLib: TGUID = '{B3B3F278-9998-4D98-B272-0042D7206501}';
  CLASS_BrowseAndDocItVBEIDETypeLib: TGUID = '{E35495C8-E19D-4FFB-BF17-04E698A47600}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IBrowseAndDocItVBEIDETypeLib = interface;
  IBrowseAndDocItVBEIDETypeLibDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  BrowseAndDocItVBEIDETypeLib = IBrowseAndDocItVBEIDETypeLib;


// *********************************************************************//
// Interface: IBrowseAndDocItVBEIDETypeLib
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B3B3F278-9998-4D98-B272-0042D7206501}
// *********************************************************************//
  IBrowseAndDocItVBEIDETypeLib = interface(IDispatch)
    ['{B3B3F278-9998-4D98-B272-0042D7206501}']
  end;

// *********************************************************************//
// DispIntf:  IBrowseAndDocItVBEIDETypeLibDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B3B3F278-9998-4D98-B272-0042D7206501}
// *********************************************************************//
  IBrowseAndDocItVBEIDETypeLibDisp = dispinterface
    ['{B3B3F278-9998-4D98-B272-0042D7206501}']
  end;

// *********************************************************************//
// The Class CoBrowseAndDocItVBEIDETypeLib provides a Create and CreateRemote method to          
// create instances of the default interface IBrowseAndDocItVBEIDETypeLib exposed by              
// the CoClass BrowseAndDocItVBEIDETypeLib. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoBrowseAndDocItVBEIDETypeLib = class
    class function Create: IBrowseAndDocItVBEIDETypeLib;
    class function CreateRemote(const MachineName: string): IBrowseAndDocItVBEIDETypeLib;
  end;

implementation

uses ComObj;

class function CoBrowseAndDocItVBEIDETypeLib.Create: IBrowseAndDocItVBEIDETypeLib;
begin
  Result := CreateComObject(CLASS_BrowseAndDocItVBEIDETypeLib) as IBrowseAndDocItVBEIDETypeLib;
end;

class function CoBrowseAndDocItVBEIDETypeLib.CreateRemote(const MachineName: string): IBrowseAndDocItVBEIDETypeLib;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_BrowseAndDocItVBEIDETypeLib) as IBrowseAndDocItVBEIDETypeLib;
end;

end.
