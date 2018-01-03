(**

  This module contains a singleton class which is automatically initialised at startup (and
  therefore should not be manually created) to intialise and register a number of global objects
  in the system.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Jan 2018

**)
Unit BADI.Initialisation;

Interface

Implementation

Uses
  {$IFDEF CODESITE}
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
  BADI.Options, 
  BADI.Base.Module;

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

Type
  TBADIDispatcherRecord = Record
    FModule    : TBaseLanguageModuleClass;
    FExt       : String;
    FCanDoc    : Boolean;
    FBlockCmt  : TCommentType;
    FLineCmt   : TCommentType;
    FInSituCmt : TCommentType;
  End;

Const
  Modules : Array[0..8] Of TBADIDispatcherRecord = (
    (FModule: TBackusNaurModule;   FExt: '.bnf';                      FCanDoc: True;  FBlockCmt: ctCPPBlock;    FLineCmt: ctCPPBlock;    FInSituCmt: ctCPPBlock),
    (FModule: TPascalModule;       FExt: '.dpk;.dpr;.pas';            FCanDoc: True;  FBlockCmt: ctPascalBlock; FLineCmt: ctPascalBlock; FInSituCmt: ctPascalBlock),
    (FModule: TCPPModule;          FExt: '.cpp;.hpp;.c;.h';           FCanDoc: True;  FBlockCmt: ctCPPBlock;    FLineCmt: ctCPPBlock;    FInSituCmt: ctCPPBlock),
    (FModule: TDFMModule;          FExt: '.dfm';                      FCanDoc: False; FBlockCmt: ctPascalBlock; FLineCmt: ctPascalBlock; FInSituCmt: ctPascalBlock),
    (FModule: TEidolonModule;      FExt: '.map';                      FCanDoc: True;  FBlockCmt: ctCPPBlock;    FLineCmt: ctCPPLine;     FInSituCmt: ctCPPBlock),
    (FModule: TTLSSchematicModule; FExt: '.schematic';                FCanDoc: False; FBlockCmt: ctCPPBlock;    FLineCmt: ctCPPLine;     FInSituCmt: ctCPPLine),
    (FModule: TINIModule;          FExt: '.ini;.tli';                 FCanDoc: True;  FBlockCmt: ctCPPBlock;    FLineCmt: ctCPPLine;     FInSituCmt: ctCPPBlock),
    (FModule: TVBModule;           FExt: '.bas;.cls;.frm';            FCanDoc: True;  FBlockCmt: ctVBLine;      FLineCmt: ctVBLine;      FInSituCmt: ctVBLine),
    (FModule: TXMLModule;          FExt: '.dtd;.htm;.html;.xml;.xsd'; FCanDoc: False; FBlockCmt: ctXML;         FLineCmt: ctXML;         FInSituCmt: ctXML)
  );

Var
  MD : TBADIDispatcher;
  iModule: Integer;

Begin
  MD := TBADIDispatcher.BADIDispatcher;  // Will create the module dispatcher
  For iModule := Low(Modules) To High(Modules) Do
    MD.Add(
      Modules[iModule].FModule,
      Modules[iModule].FExt,
      Modules[iModule].FCanDoc,
      Modules[iModule].FBlockCmt,
      Modules[iModule].FLineCmt,
      Modules[iModule].FInSituCmt);
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
