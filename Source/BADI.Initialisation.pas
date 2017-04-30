(**

  This module contains a singleton class which is automatically initialised at startup (and
  therefore should not be manually created) to intialise and register a number of global objects
  in the system.

  @Author  David Hoyle
  @Version 1.0
  @Date    30 Apr 2017

**)
Unit BADI.Initialisation;

Interface

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.Types,
  BADI.Module.Dispatcher,
  BADI.BackusNaur.Module,
  BADI.CPP.Module,
  BADI.DFM.Module,
  BADI.Eidolon.Module,
  BADI.Eidolon.TLSSchematic.Module,
  BADI.INI.Module,
  BADI.Pascal.Module,
  BADI.VB.Module,
  BADI.VB.ModuleFull,
  BADI.XML.Module,
  BADI.Options;

Type
  (** This class i responsible for initialising the BADI application objects in the correct
      order. **)
  TBADIInitialisation = Class
  Strict Private
    Class Var
      (** This is a hidden class variable to hold the instance reference for the class. **)
      FBADIInitialisationInstance: TBADIInitialisation;
  Strict Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Class Procedure InitialiseBADI;
    Class Procedure FinaliseBADI;
  End;

{ TBADIInitialisation }

(**

  This is a constructor for the TBADI Initialisation class.

  @precon  None.
  @postcon Initialises the 2 global objects ModuleDispatcher and BrowseAndDocItOptions in the
           correct order.

**)
Constructor TBADIInitialisation.Create;

Var
  MD : TBADIDispatcher;

Begin
  MD := TBADIDispatcher.BADIDispatcher;  // Will create the module dispatcher
  MD.Add(TBackusNaurModule, '.bnf', True, ctCPPBlock, ctCPPBlock, ctCPPBlock);
  MD.Add(TPascalModule, '.dpk;.dpr;.pas', True, ctPascalBlock, ctPascalBlock, ctPascalBlock);
  MD.Add(TCPPModule, '.cpp;.hpp;.c;.h', True, ctCPPBlock, ctCPPBlock, ctCPPBlock);
  MD.Add(TDFMModule, '.dfm', False, ctPascalBlock, ctPascalBlock, ctPascalBlock);
  MD.Add(TEidolonModule, '.map', True, ctCPPBlock, ctCPPLine, ctCPPBlock);
  MD.Add(TTLSSchematicModule, '.schematic', False, ctCPPBlock, ctCPPLine, ctCPPLine);
  MD.Add(TINIModule, '.ini;.tli', True, ctCPPBlock, ctCPPLine, ctCPPBlock);
  MD.Add(TVBModule, '.bas;.cls;.frm', True, ctVBLine, ctVBLine, ctVBLine);
  MD.Add(TXMLModule, '.dtd;.htm;.html;.xml;.xsd',  False, ctXML, ctXML, ctXML);
  TBADIOptions.BADIOptions.LoadSettings; // Will create the Options
End;

(**

  This is a destructor or the TBADIInitialisation class.

  @precon  None.
  @postcon Ensures the options are saved.

**)
Destructor TBADIInitialisation.Destroy;

Begin
  TBADIOptions.BADIOptions.Free;
  TBADIDispatcher.BADIDispatcher.Free;
  Inherited Destroy;
End;

(**

  This class method frees the BADI Initialisation class which in return saved the applications
  information.

  @precon  None.
  @postcon The initialisation class is freed and in doing so frees the module displatcher and
           options class.

**)
Class Procedure TBADIInitialisation.FinaliseBADI;

Begin
  FBADIInitialisationInstance.Free;
End;

(**

  This class method creates an instance of the initialisation class and in turn the module
  dispatcher (registering the modules) and the options.

  @precon  None.
  @postcon The 2 global objects in BADI (Dispatcher and Options) are intialised correctly.

**)
Class Procedure TBADIInitialisation.InitialiseBADI;

Begin
  FBADIInitialisationInstance := TBADIInitialisation.Create;
End;

(** This initialization section should be the ONLY intiialization section for the BADI code. It
    initializes the 2 global singleton class and registers all the modules with the dispatcher. **)
Initialization
  TBADIInitialisation.InitialiseBADI;
(** This frees the initialisation class. **)
Finalization
  TBADIInitialisation.FinaliseBADI;
End.
