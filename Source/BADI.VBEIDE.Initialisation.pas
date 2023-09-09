(**

  This module contains a singleton class which is automatically initialised at start-up (and
  therefore should not be manually created) to initialise and register a number of global objects
  in the system.

  @Author  David Hoyle
  @Version 1.180
  @Date    02 Sep 2023

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

**)
Unit BADI.VBEIDE.Initialisation;

Interface

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.Types,
  BADI.Module.Dispatcher,
  BADI.VB.Module,
  //: @debug BADI.VB.ModuleFull,
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
  @postcon Initialises the 2 global objects Module Dispatcher and Browse And Doc It Options in the
           correct order.

**)
Constructor TBADIInitialisation.Create;

Type
  TBADIDispatcherRecord = Record
    FModule       : TBaseLanguageModuleClass;
    FExt          : String;
    FCanDoc       : Boolean;
    FBlockCmt     : TCommentType;
    FLineCmt      : TCommentType;
    FInSituCmt    : TCommentType;
    FCommentTypes : TCommentTypes;
  End;

Const
  Modules : Array[0..0] Of TBADIDispatcherRecord = (
    (FModule: TVBModule;           FExt: '.bas;.cls;.frm';            FCanDoc: True;  FBlockCmt: ctVBLine;      FLineCmt: ctVBLine;      FInSituCmt: ctVBLine;      FCommentTypes: [ctVBLine])
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
      Modules[iModule].FInSituCmt,
      Modules[iModule].FCommentTypes
    );
  TBADIOptions.BADIOptions.LoadSettings; // Will create the Options
End;

(**

  This is a destructor or the TBADIInitialisation class.

  @precon  None.
  @postcon Ensures the options are saved.

**)
Destructor TBADIInitialisation.Destroy;

Begin
  TBADIOptions.Release;
  TBADIDispatcher.BADIDispatcher.Free;
  Inherited Destroy;
End;

(**

  This class method frees the BADI Initialisation class which in return saved the applications
  information.

  @precon  None.
  @postcon The initialisation class is freed and in doing so frees the module dispatcher and
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
  @postcon The 2 global objects in BADI (Dispatcher and Options) are initialised correctly.

**)
Class Procedure TBADIInitialisation.InitialiseBADI;

Begin
  FBADIInitialisationInstance := TBADIInitialisation.Create;
End;

(** This initialization section should be the ONLY initialization section for the BADI code. It
    initialises the 2 global singleton class and registers all the modules with the dispatcher. **)
Initialization
  TBADIInitialisation.InitialiseBADI;
(** This frees the initialisation class. **)
Finalization
  TBADIInitialisation.FinaliseBADI;
End.
